{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Laser (

  -- * Laser!

  Laser,
  runLaser,
  runLaze,
  laser,
  seekTo,
  getTime,
  getValue,
  getEvent,
  putEvent,
  getNext,
  getLength,
  (<*><*>),

  -- * MTL stuff

  get,
  put,
  modify,
  state
) where

import Control.Applicative
import Control.Monad (ap)
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS

import TimeData

-- monad for computing Time -> (V a, E b) -> s -> ((V c, E d),s)

-- Laser' s a b c d result
-- Laser a d s b c result -- this ordering satisfies MonadRWS class






-- how to implement this monad... it keeps a state consisting of a time pointer
-- the vs and es input streams which will be consumed. 

-- the algorithm to generate the two output streams will be similar to break/span
-- using returned tuples to get the stream tails and (lazy) final state

-- each step produces 1 value signal step and zero or more events and new state.
-- and a new time, and a continuation accepting input event.

-- laze returns a continuation and a new time
-- lazeTo returns a continuation without a time, expecting to go to next event or end
-- each step accepts current time
-- each step accepts state, returns new state
-- each step accepts current a
-- lazing steps return functions and events


-- A LazeFunc is a function that takes the context of a Laser program and returns whatever.
type LazeFunc s a b c d r = Time -> a -> Maybe c -> s -> r

-- LazeAction represents requests Laser can do paired with the response handler or continuation.
data LazeAction s a b c d r
  = Chlaser (Time -> a -> b) (LazeFunc s a b c d r)
  | Seek Time (LazeFunc s a b c d r)
  | PutS s (LazeFunc s a b c d r)
  | Emit d (LazeFunc s a b c d r)
  | GetLength (Time -> LazeFunc s a b c d r)
  | GetNext (Maybe Time -> LazeFunc s a b c d r)
      deriving Functor

-- A Laser program is a LazeFunc that returns an answer or a request that must be handled.
-- | DSL program to generate time dependent data.
-- 
-- This doc lists special abilities available to 'Laser' to be used with do notation,
-- @(>>=)@, or Applicative to form a procedure.
--
-- To run a 'Laser' on input to generate a frame, use 'runLaser'. To embed a frame in a
-- component arrow use 'runLaze' which takes arguments in another order.
newtype Laser s a b c d result =
  Laser { unLaser :: LazeFunc s a b c d (PureOr (LazeAction s a b c d) result) }
    deriving Functor

-- The distilled recursive skeleton of a monadic program with effect functor f
data PureOr f a = Pure a | Impure (f (PureOr f a))

instance Functor f => Functor (PureOr f) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Impure action) = Impure (fmap (fmap f) action)

instance Applicative (Laser s a b c d) where
  pure x = Laser (\_ _ _ _ -> Pure x)
  (<*>) = ap

-- This is a "free monad" construction. Which just means >>= chains together
-- two programs without doing any reductions or effects. An interpreter
-- will come and execute the effects later.
instance Monad (Laser s a b c d) where
  Laser f >>= h = Laser $ \t a mc s -> case f t a mc s of
    Pure x -> unLaser (h x) t a mc s
    Impure action -> Impure $ case action of
      GetLength k -> GetLength (\l -> (unLaser (Laser (k l) >>= h)))
      GetNext k -> GetNext (\mt -> (unLaser (Laser (k mt) >>= h)))
      Chlaser nl k -> Chlaser nl (unLaser (Laser k >>= h))
      Seek target k -> Seek target (unLaser (Laser k >>= h))
      PutS s' k -> PutS s' (unLaser (Laser k >>= h))
      Emit d k -> Emit d (unLaser (Laser k >>= h))

(<*><*>) :: V a -> (Time -> a -> b) -> Time -> b
v <*><*> f = f <*> (v `at`)


-- | Generate continuous time data and discrete event stream with a procedure.
--
-- The two output streams are generated in a way suitable for streaming, assuming the final
-- state is not inspected too early, i.e. before both streams are consumed.
runLaser
  :: Semigroup d
  => Laser s a b c d na -- ^ procedure to run
  -> Time -- ^ frame length
  -> V a  -- ^ input value signal
  -> E c  -- ^ input event stream
  -> b    -- ^ default output before any 'laser' choice is made
  -> s    -- ^ starting state
  -> (V b, E d, s) -- ^ output value signal, output events, final state
runLaser laz frameLen vin ein prior seed = go 0 (\_ _ -> prior) Nothing vin ein seed laz where
  --go :: Time -> (Time -> a -> b) -> Maybe d -> V a -> E c -> s -> Laser s a b c d na -> (V b, E d, s)
  go t laserf dout oldV oldE !s (Laser f) =
    let !vs = discardValues t oldV
        !es = discardEvents t oldE
        occ = case es of E [] -> Nothing
                         E ((et,ex):_) -> if t < et then Nothing else Just ex
    in case f t (vs `at` t) occ s of
      Pure _ -> (eternal (vs <*><*> laserf), toE t dout, s)
      Impure action -> case action of
        GetLength k -> go t laserf dout vs es s (Laser (k frameLen))
        GetNext k ->
          let answer = case es of E [] -> Nothing; E ((next,_):_) -> Just next
          in go t laserf dout vs es s (Laser (k answer))
        Chlaser newf k ->
          let (voutmore, eoutmore, s') = go t newf dout vs es s (Laser k)
          in (V (vs <*><*> laserf) t voutmore, eoutmore, s')
        Seek target k -> case dout of
          Nothing -> go target laserf Nothing vs es s (Laser k)
          Just d ->
            let (voutmore, E dmore, s') = go target laserf Nothing vs es s (Laser k)
            in (voutmore, E ((t,d):dmore), s')
        PutS s' k -> go t laserf dout vs es s' (Laser k)
        Emit d k -> go t laserf (dout <> Just d) vs es s (Laser k)

-- | This arrangement of arguments is convenient for forming a component arrow directly.
runLaze :: Semigroup d => b -> Laser s a b c d na -> Time -> (V a, E c) -> s -> ((V b, E d), s)
runLaze prior laz frameLen (vin,ein) seed =
  let (vout,eout,s') = runLaser laz frameLen vin ein prior seed in ((vout,eout),s')

-- assuming no more input ever, what's the next activity time of this laser
--hypotheticalTime :: Laser s a b c d r -> V a -> s -> Time
--hypotheticalTime (Laser f) vin seed = -- possible, requires simulating the state in such a way

{-
wrapLaser
  :: Semigroup d
  => Laser s vi vo ei eo na -- ^ procedure to run
  -> b    -- ^ default output before any 'laser' choice is made
  -> s    -- ^ starting state
  -> Incr vi ei vo eo
wrapLaser laz_0 prior seed = wrap 0 (\_ _ -> prior) Nothing seed laz_0 where
  wrap t laserf dout !s laz = Incr
    { incrV = \vi -> eternal (vi <*><*> laserf)
    , incrE = \vi -> dout
    , incrHNAT = \vi -> hypotheticalTime laz s
    , incrBurn = \vi delta -> let (laserf', s', dout', laz') = freeWheel laz vi delta s
                              in wrap (t+delta) laserf' dout' s' laz'
    , incrPut = \vi x -> let (laserf', s', dout', laz') = whenSuddenly laz vi s x
                         in wrap t laserf' dout' s' laz'
    }

  done laserf = Incr
    { incrV = \vi -> eternal (vi <*><*> laserf)
    , incrE = \_ -> Nothing
    , incrHNAT = 1/0
    , incrBurn = \_ _ -> done laserf
    , incrPut = \_ _ -> done laserf }

  -- freeWheel and whenSuddenly execute the laser program with different context
  -- until different stopping conditions:

  -- freeWheel
  whenSuddenly laz vi !s x = -- run the laser program with an input, 

    in case f t (vs `at` t) occ s of
      Pure _ -> (eternal (vs <*><*> laserf), toE t dout, s)
      Impure action -> case action of
        GetLength k -> go t laserf dout vs es s (Laser (k frameLen))
        GetNext k ->
          let answer = case es of E [] -> Nothing; E ((next,_):_) -> Just next
          in go t laserf dout vs es s (Laser (k answer))
        Chlaser newf k ->
          let (voutmore, eoutmore, s') = go t newf dout vs es s (Laser k)
          in (V (vs <*><*> laserf) t voutmore, eoutmore, s')
        Seek target k -> case dout of
          Nothing -> go target laserf Nothing vs es s (Laser k)
          Just d ->
            let (voutmore, E dmore, s') = go target laserf Nothing vs es s (Laser k)
            in (voutmore, E ((t,d):dmore), s')
        PutS s' k -> go t laserf dout vs es s' (Laser k)
        Emit d k -> go t laserf (dout <> Just d) vs es s (Laser k)
-}
        
nextEv :: E a -> Maybe (Time, a)  
nextEv (E []) = Nothing
nextEv (E ((t,x):_)) = Just (t,x)

tailE :: E a -> E a
tailE (E es) = E (drop 1 es)

toE :: Time -> Maybe a -> E a
toE _ Nothing  = E []
toE t (Just x) = E [(t,x)]

-- | 'ask' performs 'getValue'
instance MonadReader a (Laser s a b c d) where
  ask = getValue
  local g (Laser f) = Laser (\t a mc s -> f t (g a) mc s)

-- | 'tell' performs 'putEvent' but only works for 'Monoid'. 'putEvent' is more relaxed.
instance Monoid d => MonadWriter d (Laser s a b c d) where
  tell d = nullAction (Emit d)

-- | 'get', 'put', 'modify', and 'state' should be used to update the state.
instance MonadState s (Laser s a b c d) where
  get = Laser (\_ _ _ s -> Pure s)
  put s' = nullAction (PutS s')

instance Monoid d => MonadRWS a d s (Laser s a b c d) where


-- | Return the time since start of frame.
getTime :: Laser s a b c d Time
getTime = Laser (\t _ _ _ -> Pure t)

-- | Sample input at the current time.
getValue :: Laser s a b c d a
getValue = Laser (\_ a _ _ -> Pure a)

-- | Return the input event at the current time, if there is one. 
getEvent :: Laser s a b c d (Maybe c)
getEvent = Laser (\_ _ mc _ -> Pure mc)

-- | Return the time of next event, if there is one.
getNext :: Laser s a b c d (Maybe Time)
getNext = Laser (\_ _ _ _ -> Impure (GetNext (\mt _ _ _ _ -> Pure mt)))

-- | Return the length of the current frame.
getLength :: Laser s a b c d Time
getLength = Laser (\_ _ _ _ -> Impure (GetLength (\l _ _ _ _ -> Pure l)))

-- | Output an event now. Multiple outputs at the same time are merged with @(<>)@.
putEvent :: d -> Laser s a b c d ()
putEvent d = nullAction (Emit d)

-- | Change / choose laser now. Has no effect until 'seekTo' is executed. Which logically
-- begins burning continuous output over that time. If 'seekTo' happens before any 'laser'
-- is configured, the default value given to 'runLaser' is used. If the procedure ends
-- before seeking to the end of frame, the current laser continues to the end.
--
-- The laser function can sample the input signal point-wise.
laser :: (Time -> a -> b) -> Laser s a b c d ()
laser laserf = nullAction (Chlaser laserf)

-- | Move to a time point greater than current time and burn continuous output
-- with the current 'laser'. Seeking backwards or past the end of frame is an error.
seekTo :: Time -> Laser s a b c d ()
seekTo target = do
  now <- getTime
  if target <= now
    then return ()
    else nullAction (Seek target)

nullAction :: (forall r . LazeFunc s a b c d r -> LazeAction s a b c d r) -> Laser s a b c d ()
nullAction wrap = Laser (\_ _ _ _ -> Impure (wrap (\_ _ _ _ -> Pure ())))

