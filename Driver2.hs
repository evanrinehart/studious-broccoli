{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}
module Driver2 where

import Data.Time
import Control.Concurrent
import Control.Exception
import Control.Applicative

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


-- | The unreactive strategy. 'F' is given an empty input, the seed state. We request 1
-- infinite length frame, which produces 1 stream of IO actions and 1 value signal. The
-- driver dispatches the IO actions more or less on schedule while waiting for Get requests.
-- The Get requests sample the value signal at time of arrival. Other requests are ignored.
-- That's why it's unreactive.
--
-- No final state is ever created, no intermediate states are ever committed.
--
-- Once tested for errors, it might work for demos. Doesn't return on its own.
unreactiveLoop
  :: MVar (DriverRequest a b)
  -> IO Time
  -> F s (E a) (V b, E (IO ()))
  -> s
  -> IO c
unreactiveLoop inbox clock (F gen) seed = boot where
  boot = do
    let ((vs, E es),_) = gen (1/0) (E []) seed -- infinite streams, no final state
    epoch <- clock
    loop epoch vs es

  loop startOfFrame !vs !es = do
    (req, now) <- case es of
      [] -> liftA2 (,) (takeMVar inbox) clock
      (target,_):_ -> do
        now <- clock
        let dt = target - (now - startOfFrame) -- if somehow this is negative, we don't sleep
        liftA2 (,) (takeMVarWatchdog inbox (virtualTimeToMicros dt) Wake) clock

    let t = now - startOfFrame

    es' <- dispatchActions t es  -- do the IO
    let vs' = discardValues t vs -- lose old value signal segments since we won't need them

    case req of
      Get outbox -> do
        let x = vs' `at` t -- sample the value stream now
        putMVar outbox x
      _ -> return ()

    loop startOfFrame vs' es'



-- | The brute force strategy. Begin with an infinite frame to find the first event. If any.
-- Then change your mind and use a frame exactly large enough to land on that event time.
-- On wake, do any events, commit the world state with deepseq, forget past input. Repeat.
-- If woken early by a request, do any events, commit world state, forget old input,
-- insert new input, repeat. Basically 
eagerLoop
  :: MVar (DriverRequest a b)
  -> IO Time
  -> F s (E a) (V b, E (IO ()))
  -> s
  -> IO c
eagerLoop inbox click gen seed = undefined




-- | Periodic service? Pick some "sample rate" (old paper by conal calls it this). Use 1/rate
-- long frames. During the frame, dispatch events, but accumulate requests to be serviced
-- only at the end of the frame. At this time you will have a 'final' state to commit by
-- deepseqing. Service all the queued requests in order, which gives a new (quantized) input
-- stream to begin the next frame with. Repeat this process using the same 1/rate frame length.
--
-- Common in video games. As noted by conal, this adds input latency on the order of 1/rate.
periodicServiceLoop
  :: MVar (DriverRequest a b)
  -> IO Time
  -> F s (E a) (V b, E (IO ()))
  -> s
  -> IO c
periodicServiceLoop inbox click gen seed = undefined
