{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
--{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module Driver3 where

import Data.Time
import Control.Concurrent
import Control.Exception
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Data.List.NonEmpty as NE
import Data.Semigroup
import Data.Foldable

import Data.Proxy

import TimeData
import Component

-- | The driver lives in a server. External agents can issue requests to it.
data DriverRequest :: * -> * -> * where
  -- | View the object now. Receive answer in the given MVar. Has no side effect on @a@.
  Get  :: MVar o -> DriverRequest i o
  -- | Edit the object now. Expects no response.
  Put  :: i -> DriverRequest i o
  -- | Like 'Get' but also make changes in the process.
  Loot :: MVar o -> i -> DriverRequest i o
  -- | Used by watchdog to wake up for scheduled events.
  Wake :: DriverRequest i o

getReqIn :: DriverRequest i o -> Maybe i
getReqIn (Get _)    = Nothing
getReqIn (Put i)    = Just i
getReqIn (Loot _ i) = Just i
getReqIn Wake       = Nothing

-- as discussed on paper, the time number in E is the time after start of frame
-- the time in V is the time after start of frame.
-- the time argument to the functions in V are to be interpreted relative to start of frame.

-- algorithms which "play" through a frame should compute a current virtual time relative to
-- start of frame by doing now - frameStartTime. And frameStartTime is an epoch time plus
-- the size of each frame we already saw!

-- to wait for an event occurring at time t, wait t - (now - frameStart) converted to micros.

-- if microseconds is very large, just sleep for some max time like 10 seconds.
takeMVarWatchdog :: MVar a -> Int -> a -> IO a
takeMVarWatchdog inbox timeSaid x
  | timeSaid <= 0 = return x
  | otherwise = do
      let million = 1000000
      let time = min timeSaid (10 * million)
      watchdog <- forkIO (threadDelay time >> putMVar inbox x)
      y <- takeMVar inbox
      killThread watchdog
      return y

-- this is kind of broken in that t very big results in too many microseconds
virtualTimeToMicros :: Time -> Int
virtualTimeToMicros t = ceiling (t * 16666) -- 1 t = 1/60 seconds, for instance.




type Driver s i o r = ReaderT (DriverEnv s i o) (StateT (DriverState s i o) IO) r

data DriverEnv s i o = DE
  { deGen :: Time -> E i -> s -> ((V o, E (IO ())), s)
  , deClock :: IO Time
  , deInbox :: MVar (DriverRequest i o) }

data DriverState s i o = DS
  { dsAction :: [(Time,IO ())]
  , dsValues :: V o
  , dsInput :: [(Time,i)]
  , dsFrameStart :: Time -- wall time
  , dsFrameLength :: Time
  , dsPointerTime :: Time
  , dsInitialState :: s
  , dsFinalState :: s }

runDriver
  :: Driver s i o r
  -> F s (E i) (V o, E (IO ()))
  -> IO Time
  -> MVar (DriverRequest i o)
  -> s
  -> IO r
runDriver proc (F gen) clock inbox seed = do
  now <- clock
  let env = DE {deGen=gen, deClock=clock, deInbox=inbox}
  let sta = DS {dsInput=[], dsFrameStart=now, dsPointerTime=0, dsInitialState=seed}
  (r, _) <- runStateT (runReaderT proc env) sta
  return r
  
timeInsert :: Semigroup a => Time -> a -> [(Time,a)] -> [(Time,a)]
timeInsert target x [] = [(target,x)]
timeInsert target x ((t,y):more) = case compare target t of
  LT -> (target,x):(t,y):more
  EQ -> (t,y <> x):more
  GT -> (t,y):timeInsert target x more

        

    



-- | Drop anything in the output streams strictly before the given time.
discard :: (Time -> Bool) -> Driver s i o ()
discard p = do
  eout <- gets dsAction
  vout <- gets dsValues
  let (eout',vout') = parallelDiscard p eout vout
  modify (\sta -> sta {dsAction=eout', dsValues=vout'})

-- | Drop anything in the output streams strictly before the given time.
-- also execute it all.
execute :: (Time -> Bool) -> Driver s i o ()
execute p = do
  eout <- gets dsAction
  vout <- gets dsValues
  (eout',vout') <- liftIO (parallelDiscardX p eout vout)
  modify (\sta -> sta {dsAction=eout', dsValues=vout'})

parallelDiscard
  :: (Time -> Bool)
  -> [(Time, IO ())]
  -> V b
  -> ([(Time, IO ())], V b)
parallelDiscard p [] ys@(V _ t2 ymore) = if p t2 then parallelDiscard p [] ymore else ([],ys)
parallelDiscard p xs@((t1,_):xmore) ys@(V _ t2 ymore) = case compare t1 t2 of
  LT -> if p t1 then parallelDiscard p xmore ys else (xs,ys)
  EQ -> if p t1 then parallelDiscard p xmore ymore else (xs,ys)
  GT -> if p t2 then parallelDiscard p xs ymore else (xs,ys)

parallelDiscardX
  :: (Time -> Bool)
  -> [(Time, IO ())]
  -> V b
  -> IO ([(Time, IO ())], V b)
parallelDiscardX p [] ys@(V _ t2 ymore) = if p t2 then parallelDiscardX p [] ymore else return ([],ys)
parallelDiscardX p xs@((t1,io):xmore) ys@(V _ t2 ymore) = case compare t1 t2 of
  LT -> if p t1 then io >> parallelDiscardX p xmore ys else return (xs,ys)
  EQ -> if p t1 then io >> parallelDiscardX p xmore ymore else return (xs,ys)
  GT -> if p t2 then parallelDiscardX p xs ymore else return (xs,ys)

appendIn :: Semigroup i => Time -> i -> Driver s i o ()
appendIn t x = do
  input <- gets dsInput
  modify (\sta -> sta { dsInput = timeInsert t x input })

clearIn :: Driver s i o ()
clearIn = do
  modify (\sta -> sta { dsInput = [] })

sampleOut :: Time -> Driver s i o o
sampleOut t = do
  v <- gets dsValues
  return (v `at` t)

getRequest :: Time -> Driver s i o (DriverRequest i o, Time)
getRequest d = if d <= 0
  then do
    now <- getPointerTime
    return (Wake, now)
  else do
    clock <- asks deClock
    inbox <- asks deInbox
    let usec = virtualTimeToMicros d
    req <- liftIO (takeMVarWatchdog inbox usec Wake)
    wallNow <- liftIO clock
    wallBase <- gets dsFrameStart
    let t = wallNow - wallBase
    modify (\sta -> sta {dsPointerTime = t})
    return (req, t)

getFrameStart :: Driver s i o Time
getFrameStart = gets dsFrameStart

regenerate :: Time -> Driver s i o ()
regenerate l = do
  gen <- asks deGen
  input <- gets dsInput
  s <- gets dsInitialState
  let ((vout,E eout),finalState) = gen l (E input) s
  modify (\sta -> sta {dsAction=eout,dsValues=vout,dsFrameLength=l,dsFinalState=finalState})

-- | Evaluate the final state in preparation for moving to a new frame. The frame start time
-- is bumped up by frame size and the new initial state is the old final state.
-- Immediately following 'commit' some driver state is invalid and the algorithm should
-- 'regenerate' before doing anything else.
commit :: Driver s i o ()
commit = do
  base  <- gets dsFrameStart
  l     <- gets dsFrameLength
  final <- gets dsFinalState
  _ <- liftIO (evaluate final) -- !
  modify (\sta -> sta {dsFrameStart=base + l, dsInitialState=final})

getPointerTime :: Driver s i o Time
getPointerTime = gets dsPointerTime

nextActivityTime :: Driver s i o Time
nextActivityTime = do
  input <- gets dsInput
  vout <- gets dsValues
  eout <- gets dsAction
  let t1 = case input of [] -> 1/0; ((t,_):_) -> t
  let t2 = case eout  of [] -> 1/0; ((t,_):_) -> t
  let t3 = let V _ t _ = vout in t
  return (minimum [t1,t2,t3])

respond :: MVar a -> a -> Driver s i o ()
respond outbox x = liftIO (putMVar outbox x)



{-
discard :: Time -> Driver i o ()
execute :: Time -> Driver i o ()
appendIn :: Time -> i -> Driver i o ()
clearIn :: Driver i o ()
sampleOut :: Time -> Driver i o o
getRequest :: Time -> Driver i o (Maybe (Request i o), Time)
regenerate :: Time -> Driver i o ()
commit :: Driver i o () -- move final state to start state and evaluate it, set time variables
getPointerTime :: Driver i o Time
nextActivityTime :: Driver i o Time
respond :: MVar a -> a -> Driver i o ()
-}



-- Driver Algorithm

-- begin by executing any event being outputted right now, by feeding object initial input events
-- pointer time t starts at 0
-- check hypothetical next activity time t'
-- get request with timeout t' - t
-- we wake up at time t.
-- if t < t', then there was "no activity" up to this point.
-- on Get, just sample the output at time t.
-- on Put, you must "open" by providing time t, then the new input, so we are shut again.
-- on Loot, sample at time t first, then do the effects of looting, so you are shut.
-- after a put or loot, there may be more outputs to execute.
-- repeat 

-- if t > t', then we passed the predicted activity time, but we know no input happened < t.
-- repeatedly open and shut while executing outputs. Note that any requests made by the app
-- will have to wait virtual time before responding.
-- When present time is reached, do the request procedure.
-- repeat


-- Offline algorithm
-- example, the output type is a column of colors. The x axis represents time. The input signal
-- is some numeric function of time. The output area has width w. Then we can visualize a
-- p :: Open (Time -> R) (Time -> Y -> Color)
-- by doing p `tossFood` f where f :: Time -> R, which gives us a Shut containing a Time -> Y -> Color
-- Then we can sample this function through Time, as long as we don't go past HAT, also in shut.
-- We can either stop there or repeat the process. We should repeat the process. So the complete
-- driver would be like

-- offline :: Open (Time -> a) (Time -> b) -> (Time -> a) -> Time -> b
-- offline p f t traverses the algorithm until we reach the segment containing t, then samples it.
-- we can be more efficient by using a fold.

-- segments :: Open (Time -> a) (Time -> b) -> (Time -> a) -> V b

-- I see now that we always assume the input source never advertises an "activity time"
-- this construct appears only in combinations of components, who output this data about
-- themselves only. It's an output, not an input.

-- So... the segments function actually makes sense for Open in general...
-- segments' :: Open a b -> a -> PWK b
-- well without Time -> a we don't know how to do time, so the output is PWK.
-- segments :: Open (Time -> a) (Time -> b) -> (Time -> a) -> V b

-- what about event streams
-- crunch :: Open (Maybe a) (Maybe b) -> E a -> E b
-- in this case, the input does have next activity, but open doesn't know about it.
-- it's fine, we interrupt it constantly, but also collect it's spontaneous output.

-- what about "constant" systems that don't use input
-- Open () b -> PWK b
-- Open () (Time -> b) -> V b
-- Open () (Maybe b) -> E b

-- we're treating V, B, Time ->, and Maybe very differently.
-- so basically, the Open type is pragmatic but has no symantics.

-- in Open a b, the types must symantically support "Sampling". I.e. 
-- 
-- segments :: Open (Time -> a) (Time -> b) -> V a -> V b
-- crunch   :: Open (Maybe a)   (Maybe b)   -> E a         -> E b
-- multicr  :: Open (f a, g b) (f a, g b) -> (f' a, g' b) -> (f' a, g' b)


-- a component has a set of push inputs and pull inputs
-- also, it has a set of push outputs and pull outputs.

-- this means given a Time -> a of pull inputs, and sequence of push input events
-- you get a Time -> b pull output and sequence of push outputs.

-- Open i o, it's a thing that eats i and spits out o.
-- Open (a,b) (c,d)

-- driver is expecting to know
-- 1. how to do put x as input, i.e. pass in Just x
-- 2. how to sample the output, i.e. v `at` t
-- 3. how to identify discrete output, i.e. when some part of it is Just
-- 4. where where the 'app' get any continuous input, ?



{-



class Chunky t where
  type ChunkOf t :: *
  subdiv :: Proxy t -> Time -> ChunkOf t -> (ChunkOf t, ChunkOf t)
  fromStream :: TS (ChunkOf t) -> t
  toStream   :: t -> TS (ChunkOf t)

instance Chunky (K a) where
  type ChunkOf (K a) = a
  subdiv _ _ x = (x,x)
  fromStream ts = K ts
  toStream (K ts) = ts

instance Chunky () where
  type ChunkOf () = ()
  subdiv _ _ _ = ((),())

instance Chunky (TF a) where
  type ChunkOf (TF a) = Time -> a
  subdiv _ _ tf = (tf, tf) -- tf taking absolute time, and domain bounds not represented

instance Chunky (E a) where
  type ChunkOf (E a) = Maybe a
  subdiv _ _ Nothing  = (Nothing, Nothing)
  subdiv _ _ (Just x) = (Just x, Nothing)

  fromStream ts = E $ go (-1/0) ts where
    go t (TS Nothing l more)  = if isInfinite l then [] else go l more
    go t (TS (Just x) l more) = if isInfinite l then [(t,x)] else (t,x):go l more

  toStream (E es) = case es of
    [] -> TS Nothing (1/0) (error "end of time")
    (t,x):more -> TS Nothing t (go x more)
      where go prev [] = TS (Just prev) (1/0) (error "end of time")
            go prev ((t,x):more) = TS (Just prev) t (go x more)

instance (Chunky a, Chunky b) => Chunky (a,b) where
  type ChunkOf (a,b) = (ChunkOf a, ChunkOf b)
  subdiv _ t (a,b) =
    let (a1,a2) = subdiv (Proxy::Proxy a) t a
        (b1,b2) = subdiv (Proxy::Proxy b) t b
    in ((a1,b1),(a2,b2))



data WireNest = WireNest
  { wn1 :: Maybe Int
  , wn2 :: Char
  , wn3 :: Time -> Double
  , wn4 :: ()
  }

data SomeFmt = SomeFmt
  { ws1 :: E Int
  , ws2 :: K Char
  , ws3 :: TF Double
  , ws4 :: ()
  }

instance Chunky SomeFmt where
  type ChunkOf SomeFmt = WireNest
  subdiv :: Proxy SomeFmt -> Time -> WireNest -> (WireNest, WireNest)
  subdiv Proxy t (WireNest a b c d) =
    let (a1,a2) = subdiv (Proxy::Proxy (E Int)) t a
        (b1,b2) = subdiv (Proxy::Proxy (K Char)) t b
        (c1,c2) = subdiv (Proxy::Proxy (TF Double)) t c
        (d1,d2) = subdiv (Proxy::Proxy ()) t d
    in (WireNest a1 b1 c1 d1, WireNest a2 b2 c2 d2)
  

runCom :: forall i o . (Chunky i, Chunky o) => Com i o -> TS (ChunkOf i) -> TS (ChunkOf o)
runCom (Com mach) (TS i t1 more) =
  let Pute o t2 k = mach i in
  case compare t1 t2 of
    LT -> let (l,_) = subdiv (Proxy::Proxy o) t1 o in TS l t1 (runCom (k t1) more)
    GT -> let (_,r) = subdiv (Proxy::Proxy i) t2 i in TS o t2 (runCom (k t2) (TS r t1 more))
    EQ -> TS o t1 (runCom (k t1) more)

exx :: Com () (K Int, E Int)
exx = Com (\() -> Pute (2+1, Nothing) (1/0) (const exx))


instance Applicative (Com i) where
  pure x = let loop = Com (\_ -> Pute x (1/0) (const loop)) in loop
  Com ff <*> Com xx = Com yy where
    yy i = let Pute a1 b1 c1 = ff i
               Pute a2 b2 c2 = xx i
           in Pute (a1 a2) (min b1 b2) (\t -> c1 t <*> c2 t)


-- a backward time stream with a complete, infinite, constant past
prelude :: a -> TU a
prelude x = TU ((x, 0) : go 0) where
  go p = (x, -(2 ** p)) : go (p+1)

-- a function of time
newtype TF a = TF (Time -> a)

-- this time data remains constant over a chunk
newtype K a = K (TS a)

-- TS (Time -> a) and TS (Maybe a) are isomorphic to B a and E a

-- therefore, if we can construct, transform, and deconstruct TS (Time -> a)
-- and TS (Maybe a), we technically have an FRP

-- there are other kinds of time data, each with their own chunk type.


-}
