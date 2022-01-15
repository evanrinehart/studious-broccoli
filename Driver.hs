{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}
module Driver (
  enterObjectLoop,
  enterLivingLoop,
  DriverRequest(..),
  LifeFunctions(..),
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

data LifeFunctions a = LifeFunctions
  { getLifeLength     :: a -> Time
  , getActionChain    :: a -> [(Time, IO ())] -- ^ hypothetical spontaneous action schedule
  , getQuitFlag       :: a -> Bool -- ^ if true driver ends the loop and returns
  , performCut        :: Time -> a -> (a,a) -- ^ cut point must be within interior of the @a@
  , growMoreLife      :: Time -> a -> a
  , convertTime1      :: WallTime -> Time -- ^ amount of real time to virtual time
  , convertTime2      :: Time -> Int -- ^ virtual time length to real microseconds
  , minChunkSize      :: Time -- ^ minimum size of new chunks of life
  }

-- | The driver lives in a server. External agents can issue requests to it.
data DriverRequest :: * -> * where
  -- | View the object now. Receive answer in the given MVar. Has no side effect on @a@.
  Get  :: MVar a -> (obj -> a) -> DriverRequest obj
  -- | Edit the object now. Expects no response.
  Put  :: (obj -> obj) -> DriverRequest obj
  -- | Like 'Get' but also make changes in the process.
  Loot :: MVar a -> (obj -> (a, obj)) -> DriverRequest obj
  -- | Used by watchdog to wake up for scheduled events.
  Wake :: DriverRequest obj

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
enterLivingLoop
  :: Show obj => IO WallTime -- ^real time source — should return monotonic time
  -> LifeFunctions obj -- ^what the driver uses to work
  -> MVar (DriverRequest obj) -- ^put requests here from another thread to interact with simulation
  -> obj -- ^the initial state
  -> IO () -- ^loop ends when drivee signals the quit flag
enterLivingLoop wallClock api inbox start = boot where
  boot = do
    let chain = getActionChain api start
    now <- wallClock
    loop now chain start 0
  loop baseTime oldChain oldObj n = do
    if getQuitFlag api oldObj
      then return ()
      else do
        let targetTime = minimum (getLifeLength api oldObj : map fst (take 1 oldChain))

        print ("take or sleep until", targetTime)
        req <- takeMVarWatchdog inbox (convertTime2 api targetTime) Wake

        now <- wallClock
        let cutTime = convertTime1 api (now `diffWallTime` baseTime)
        let endTime = getLifeLength api oldObj
        -- if cutTime is too ridiculously small, esp zero, we may have problems

        when (cutTime < 0.001) $ do
          putStrLn ("loop reoccurred very soon: " ++ show (now, baseTime, now `diffWallTime` baseTime, cutTime))

        print (SD baseTime now (getLifeLength api oldObj) 
          cutTime (getQuitFlag api oldObj) (map (\(x,_) -> (x,())) oldChain) (show oldObj) n)
        -- case 1, t is before lifetime is up, go to next step.
        -- case 2, ran out of life. Finish remaining sigmas, grow, shift, discard, ready for next step.
        (oldObj',t,oldChain',n') <- if cutTime < endTime
          then do
            return (oldObj, cutTime, oldChain, n)
          else do
            executeWholeChain oldChain
            let chunkSize = minChunkSize api
            let newCutTime = cutTime - endTime
            let numChunks = ceiling (newCutTime / chunkSize) :: Int -- hmm
            putStrLn ("GROW " ++ show numChunks)
            let newObj = growMoreLife api (fromIntegral numChunks * chunkSize) oldObj
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
enterObjectLoop
  :: MVar (DriverRequest obj) -- ^put requests here from another thread to interact with the object
  -> obj -- ^the initial state
  -> IO a -- ^loop never ends 
enterObjectLoop inbox obj = do
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

  enterObjectLoop inbox obj'

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

ex :: LifeFunctions Box
ex = LifeFunctions
  { getLifeLength = boxLen
  , getActionChain = boxChain
  , getQuitFlag  = boxQuit
  , performCut   = boxCut
  , growMoreLife = boxGrow
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

  _ <- forkIO $ do
    threadDelay 1434567
    putMVar var (Get sideVar (\(Box l) -> l))
    l <- takeMVar sideVar
    threadDelay 100000
    print l

    threadDelay 1434567
    putMVar var (Put (\(Box _) -> Box (-1)))

    return ()
  enterLivingLoop clock ex var (Box (minChunkSize ex))

{-
data NonFlat a = Exactly !a | Over !a (NonFlat a)
  deriving Show

type Time' = NonFlat Time

mergeStreams :: Semigroup a => [(Time',a)] -> [(Time',a)] -> [(Time',a)]
mergeStreams xs [] = xs
mergeStreams [] ys = ys
mergeStreams ((t1,x):xs) ((t2,y):ys) = case compare t1 t2 of
  LT -> (exact t1,x) : mergeStreams xs ((improve t1 t2, y):ys)
  EQ -> (t1,x <> y) : mergeStreams xs ys
  GT -> (exact t2,y) : mergeStreams ((improve t2 t1,x):xs) ys

-- attempt to get the exact value, might be expensive, or even undefined
exact :: NonFlat a -> NonFlat a
exact (Exactly t)   = Exactly t
exact (Over _ more) = exact more

-- attempt to get the exact value, might be expensive, or even undefined
flat :: NonFlat a -> a
flat (Exactly t)   = t
flat (Over _ more) = flat more

-- partially improve a NonFlat
improve :: Ord a => a -> NonFlat a -> NonFlat a
improve limit (Exactly t) = Exactly t
improve limit (Over p more) = if limit <= p then seekNonFlat limit more else Over p more

instance Ord a => Ord (NonFlat a) where
  Exactly t1 < Exactly t2 = t1 < t2
  Exactly t1 < Over t2 more = if t1 <= t2 then True else Exactly t1 < more
  Over t1 more < Exactly t2 = if t2 <= t1 then False else more < Exactly t2
  Over t1 more1 < Over t2 more2 = more1 < more2

type E a = [(Time,a)]
type F s a b = Time -> a -> s -> (b,s)
-}

-- in this type, sampling at t=0 always gives the current value
-- to move ahead you must "burn" some amount
-- The time field is size of this segment (or infinite)
