{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
module Driver (

  -- | Driver thread for a thing that is
  --
  --    * pollable at any time
  --    * editable at any time
  --    * autonomously performing actions at times
  --

  enterRealTimeLoop,
  enterStasisLoop,
  DriverRequest(..),
  DriveeAPI(..),
  WallTime(..),
  newClock
) where

import Data.Time
import Control.Concurrent
import Control.Monad (when)
import Control.Exception

import Film


-- four kinds of driver interactions I noticed:
-- 'pi' (push) in which something sends a message to the subject 'fire and forget'
-- 'sigma' (spontaneous) suddenly the system wants to send a message 'fire and forget' *now*
-- 'delta' (demand/request) something wants to query the subject *now*
-- 'rho' (read-only) this is like delta but not intended to cause any effect

data DriveeAPI a = DriveeAPI
  { getFilmLength     :: a -> Time
  , getActionChain    :: a -> [(Time, IO ())] -- ^ hypothetical spontaneous action schedule
  , getQuitFlag       :: a -> Bool -- ^ if true driver ends the loop and returns
  , performCut        :: Time -> a -> (a,a) -- ^ cut point must be within interior of the @a@
  , growMoreFilm      :: Time -> a -> a
  , convertTime1      :: WallTime -> Time -- ^ amount of real time to virtual time
  , convertTime2      :: Time -> Int -- ^ virtual time length to real microseconds
  , minChunkSize      :: Time -- ^ minimum size of new chunks of film
  }

-- | The driver lives in a server. External agents can issue requests to it.
-- Type @a@ is the object type.
data DriverRequest :: * -> * where
  -- | View the object now. Receive answer in the given MVar. Has no side effect on @a@.
  Get  :: MVar r -> (a -> r) -> DriverRequest a
  -- | Edit the object now. Expects no response.
  Put  :: (a -> a) -> DriverRequest a
  -- | Like 'Get' but also make changes in the process.
  Loot :: MVar r -> (a -> (r, a)) -> DriverRequest a
  -- | Used by watchdog to wake up for scheduled events.
  Wake :: DriverRequest a 

-- | Value corresponding to real time. 1 = 1 second or 1 second since an arbitrary time.
newtype WallTime = WallTime Double deriving (Eq,Ord,Read,Show)

-- | Like 'takeMVar' but spawns a watchdog who waits (at least) the given number of microseconds
-- then puts the given value in the var. Either way, the watchdog thread is killed before returning.
--
-- This is a poor man's timeout. Note the watchdog may put its value after you take a normal value
-- but before it is subsequentialy killed. This manifests as performing another maintenance loop
-- almost immediately. Because of this be prepared for very small or zero deltas.
takeMVarWatchdog :: MVar a -> Int -> a -> IO a
takeMVarWatchdog inbox time x = do
  watchdog <- forkIO (threadDelay time >> putMVar inbox x)
  y <- takeMVar inbox
  killThread watchdog
  return y

newClock :: IO (IO WallTime)
newClock = do
  epoch <- getCurrentTime
  return $ do
    now <- getCurrentTime
    let delta = realToFrac (now `diffUTCTime` epoch)
    return (WallTime delta)
    

data ServerDebug = SD
  { sdBaseTime :: WallTime
  , sdCurrentTime :: WallTime
  , sdLen :: Time
  , sdCutTime :: Time
  , sdQuit :: Bool
  , sdChain :: [(Time,())]
  , sdSubject :: String
  , sdChunkNo :: Int }
      deriving Show

-- | Begin running a simulation in time and responding to external requests.
enterRealTimeLoop
  :: Show a => IO WallTime -- ^real time source — should return monotonic time
  -> DriveeAPI a -- ^what the driver uses to work
  -> MVar (DriverRequest a) -- ^put requests here from another thread to interact with simulation
  -> a -- ^the initial state
  -> IO () -- ^loop ends when drivee signals the quit flag
enterRealTimeLoop wallClock api inbox start = boot where
  boot = do
    let chain = getActionChain api start
    now <- wallClock
    loop now chain start 0
  loop baseTime oldChain oldObj n = do
    if getQuitFlag api oldObj
      then return ()
      else do
        let targetTime = minimum (getFilmLength api oldObj : map fst (take 1 oldChain))

        print ("take or sleep until", targetTime)
        req <- takeMVarWatchdog inbox (convertTime2 api targetTime) Wake

        now <- wallClock
        let cutTime = convertTime1 api (now `diffWallTime` baseTime)
        let endTime = getFilmLength api oldObj
        -- if cutTime is too ridiculously small, esp zero, we may have problems

        when (cutTime < 0.001) $ do
          putStrLn ("loop reoccurred very soon: " ++ show (now, baseTime, now `diffWallTime` baseTime, cutTime))

        print (SD baseTime now (getFilmLength api oldObj) 
          cutTime (getQuitFlag api oldObj) (map (\(x,_) -> (x,())) oldChain) (show oldObj) n)
        -- case 1, t is before lifetime is up, go to next step.
        -- case 2, ran out of film. Finish remaining sigmas, grow, shift, discard, ready for next step.
        (oldObj',t,oldChain',n') <- if cutTime < endTime
          then do
            return (oldObj, cutTime, oldChain, n)
          else do
            executeWholeChain oldChain
            let chunkSize = minChunkSize api
            let newCutTime = cutTime - endTime
            let numChunks = ceiling (newCutTime / chunkSize) :: Int -- hmm
            putStrLn ("GROW " ++ show numChunks)
            let newObj = growMoreFilm api (fromIntegral numChunks * chunkSize) oldObj
            let newChain = getActionChain api newObj
            return (newObj, newCutTime, newChain, n + numChunks)

        let (_, obj) = performCut api t oldObj'
        sigmas <- shiftL t <$> executeChainUpTo t oldChain'

        -- now obj, sigmas, at 0 are current, and there may be events at t = 0 waiting.
        -- and the request may cause them to not happen, so wait to execute those
        
        (obj',sigmas') <- case req of
          Get outbox f -> do
            putStrLn "GET"
            putMVar outbox (f obj) -- lazy!
            return (obj, sigmas)
          Put edit -> do
            putStrLn "PUT"
            let obj' = edit obj
            -- do repeat yourself
            let newChain = getActionChain api obj'
            return (obj', newChain)
          Loot outbox act -> do
            putStrLn "LOOT"
            let (x, obj') = act obj
            putMVar outbox x
            -- do repeat yourself
            let newChain = getActionChain api obj'
            return (obj', newChain)
          Wake -> do
            putStrLn "WAKE"
            return (obj, sigmas)

        sigmas'' <- executeChainNowOnly sigmas'
        loop now sigmas'' obj' n'

-- | A server loop which ignores real time. The served object is static until acted on.
enterStasisLoop
  :: MVar (DriverRequest a) -- ^put requests here from another thread to interact with the object
  -> a -- ^the initial state
  -> IO b -- ^loop never ends 
enterStasisLoop inbox obj = do
  req <- takeMVar inbox

  obj' <- case req of
    Get outbox f -> do
      putMVar outbox (f obj) -- lazy!
      return obj
    Put edit -> do
      let obj' = edit obj
      evaluate obj'
      return obj'
    Loot outbox act -> do
      let (x, obj') = act obj
      putMVar outbox x -- x is not evaluated by me
      evaluate obj'    -- but new object is
      return obj'
    _ -> do
      return obj

  enterStasisLoop inbox obj'

type ActionChain = [(Time, IO ())]

-- | Launch all the missiles up to (less than) some point in time. Return the remainder.
executeChainUpTo :: Time -> ActionChain -> IO ActionChain
executeChainUpTo limit [] = return []
executeChainUpTo limit ((t,io) : more) = case compare t limit of
  LT -> do
    io
    executeChainUpTo limit more
  EQ -> return more
  GT -> return more

-- | Only execute actions if their ETA = 0. Return the unused rest of the chain.
executeChainNowOnly :: ActionChain -> IO ActionChain
executeChainNowOnly [] = return []
executeChainNowOnly chain@((t,io):more) = case compare t 0 of 
  LT -> error ("(bug) executeChainNowOnly: " ++ show (map fst chain))
  EQ -> io >> return more
  GT -> return chain

-- | Execute all the actions immediately.
executeWholeChain :: ActionChain -> IO ()
executeWholeChain = mapM_ snd

-- | should satisfy @soonest es = t   =>   soonest (shiftL t es) = 0@
shiftL :: Double -> ActionChain -> ActionChain
shiftL dt [] = []
shiftL dt ((t,x):more) = (t - dt, x) : shiftL dt more

diffWallTime :: WallTime -> WallTime -> WallTime
diffWallTime (WallTime x) (WallTime y) = WallTime (x - y)


data Box = Box Double deriving Show

boxLen (Box l ) = l
boxChain (Box l) = []
boxQuit (Box l) = l < 0
boxCut t (Box l) = (Box t, Box (l - t))
boxGrow l2 (Box l1) = Box l2

ex :: DriveeAPI Box
ex = DriveeAPI
  { getFilmLength = boxLen
  , getActionChain = boxChain
  , getQuitFlag  = boxQuit
  , performCut   = boxCut
  , growMoreFilm = boxGrow
--  , convertTime1 = \(WallTime t) -> t -- wall to virtual time conversion
  , convertTime1 = \(WallTime t) -> t * 60 -- 60 'virtual time' per second
  , convertTime2 = \t -> ceiling (t * 16666.666)
  , minChunkSize = 30
  }
  

test :: IO ()
test = do
  clock <- newClock
  var <- newEmptyMVar
  sideVar <- newEmptyMVar

  forkIO $ do
    threadDelay 1434567
    putMVar var (Get sideVar (\(Box l) -> l))
    l <- takeMVar sideVar
    threadDelay 100000
    print l

    threadDelay 1434567
    putMVar var (Put (\(Box _) -> Box (-1)))

    return ()
  enterRealTimeLoop clock ex var (Box (minChunkSize ex))
