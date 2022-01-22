{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
module NomYex where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Category
import GHC.Generics
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Control.Applicative
import Data.Profunctor
import Data.Void

import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent

type Time = Double

-- this is the simple version of Nom/Yex
-- nomming returns a single yex. Composing Noms causes nomming to
-- produce the minimum size yex (minyex), which by default will cause many components
-- to cause extraneous fragmentation to each other, resulting in processing many
-- "non occurrences".

-- But the instances for basic classes are very simple. And we might have some combinators
-- to help isolate components from each others fragmentation.

-- | Causal transducer of timed sequence of symbols to be used as a proxy for synchronous
-- deterministic continuous time dataflow programming.
--
-- \[
-- ⟦{\tt Nom\ i\ o}⟧ = {\tt [(i, Time)]} \xrightarrow[causal]{} {\tt [(o,Time)]}
-- \]
--
-- Here @i@ and @o@ meant to be instances of 'Sym'. The paired 'Time' is the fragment length.
-- The symbol in each fragment represent a continuum of values (or lack thereof) over
-- the given time span. Which may be infinite.
--
-- A given 'Nom' consumes 1 symbol (but not the input fragment size) and outputs 1 fragment
-- (including symbol and size) along with a continuation (see 'Yex'). The continuation goes three ways:
-- the input was shorter than the output, the output was shorter than the input, or there
-- was a tie. This lets the Nom react to being interrupted by new information if it wants to.
--
-- The input size is not immediately provided with the input symbol because of the causal restriction.
-- In general we don't know that information yet.
--
-- Some symbol types (e.g. 'V') represent varying values. Valid ways for a 'Nom' to compute with these
-- include: sampling at the current time, outputting a varying value that is a causally
-- transformed version of the current input, or a causally transformed version of a past fragment.
--
-- Mistakenly using acausal transformations or sampling, using future information for a result now,
-- can produce confusing results. Not officially supported.
--
-- Coherence: Many fragmentations, when glued together, produce the same behavior.
-- For 'Nom's to be construed as a proxy for causal transformation of behaviors, then the results
-- shouldn't depend on the particulars of the fragmentation. This module unfortunately provides
-- no real help in this department. Some basic advice, don't respond to non-occurrences of events
-- at a time you otherwise wouldn't have done anything. Place a sentry to isolate inner 'Nom's from
-- fragmentation when nothing of interest has happened, which should also improve performance.
data Nom i o = Nom { stepNom :: i -> Yex i o } deriving Functor

-- | A chunk of output from a 'Nom' paired with a continuation. The losing continuation
-- is only valid for times less than yexSize.
data Yex i o = Yex
  { yexOut  :: o
  , yexSize :: Time
  , yexNext :: WinLose -> Nom i o }
      deriving Functor

instance Show o => Show (Yex i o) where
  showsPrec d (Yex o size _) = showParen (d > 10) $
    ( showString "Yex "
    . showsPrec 11 o
    . showString " "
    . shows size
    . showString " k" )

data WinLose
  = Win  -- ^ The output chunk was shorter than the input chunk.
  | LoseAt Time -- ^ Input chunk ended first (time given) and previous output was truncated.
  | Tie -- ^ The output chunk and input chunk ended simultaneously.
      deriving (Eq,Ord,Show,Read)

-- | Parallel composition.
instance Applicative (Nom i) where
  pure x = let go = Nom (\_ -> Yex x (1/0) (const go) ) in go
  Nom ff <*> Nom xx = Nom (liftA2 (<*>) ff xx)

instance Applicative (Yex i) where
  pure x = Yex x (1/0) (const (pure x))
  Yex f size1 k1 <*> Yex x size2 k2 = Yex (f x) (min size1 size2) (liftA2 (<*>) k1 k2)

-- | Lift (causal) functions on symbols to a stateless transducer.
instance Arrow Nom where
  arr f = let go = Nom (\i -> Yex (f i) (1/0) (const go)) in go
  Nom f *** Nom g = Nom $ \(i1,i2) ->
    let Yex a1 b1 c1 = f i1
        Yex a2 b2 c2 = g i2
    in Yex (a1,a2) (min b1 b2) (liftA2 (***) c1 c2)

-- | Serial composition.
instance Category Nom where
  id = let go = Nom (\i -> Yex i (1/0) (const go)) in go
  Nom f . Nom g = Nom $ \i ->
    let Yex a1 b1 c1 = g i
        Yex a2 b2 c2 = f a1
    in Yex a2 (min b1 b2) (liftA2 (.) c2 c1)

-- | Pre and post processing of chunks.
instance Profunctor Nom where
  lmap g (Nom nom) = Nom (\i -> let Yex x s k = nom (g i) in Yex x s (lmap g . k))
  rmap f (Nom nom) = Nom (\i -> let Yex x s k = nom i in Yex (f x) s (fmap f . k))

-- | When @c@ is an instance of 'Sym', a @Tape c@ can be interpreted as
-- a time varying haskell value slice. The type of the slice depends on @c@.
--
-- @
-- slice :: SymType -> Type
-- slice (V a) = a
-- slice (K a) = a
-- slice (E a) = Maybe a
-- slice () = ()
-- slice (c,d) = (slice c, slice d)
-- slice (c -> d) = c -> d*
-- @
--
-- where @a@ is any haskell type and @c@, @d@ are symbol types.
-- 
-- \* The function symbol represents non-varying function on symbols, not arbitrary haskell types.
--   Use @K (a -> b)@ or @V (a -> b)@ for that.
newtype Tape a = Tape [(a,Time)] deriving (Show,Functor)

-- | Consider a 'Nom'@ i o@ to be a causal transducer of 'Tape's.
transduce :: (Sym i, Sym o) => Nom i o -> Tape i -> Tape o
transduce n (Tape xs) = Tape (go n xs) where
  go (Nom _) [] = []
  go (Nom nom) ((i,isize):more) =
    let Yex o osize k = nom i in
    case compare osize isize of
      LT -> (o,osize) : go (k Win) ((burn osize i, isize - osize):more)
      EQ -> (o,osize) : go (k Tie) more
      GT -> (o,isize) : go (k (LoseAt isize)) more

-- | Feedback composition. The output chunk is immediately available on 2nd input channel.
-- The resulting 'Nom' may be more or less undefined.
feedback :: Nom (i,o) o -> Nom i o
feedback (Nom nom) = Nom (\i -> let Yex o size k = nom (i,o) in Yex o size (feedback . k))

-- | To negate or eliminate (something, such as an element in a dialectic process) but
-- preserve as a partial element in a synthesis.
sublate :: Nom i o -> Nom o i -> Void
sublate (Nom n1) (Nom n2) =
  let Yex o size1 k1 = n1 i
      Yex i size2 k2 = n2 o in
  case compare size1 size2 of
    LT -> let n1' = k1 Win; n2' = k2 (LoseAt size1) in sublate n1' n2'
    EQ -> let n1' = k1 Tie; n2' = k2 Tie            in sublate n1' n2'
    GT -> let n1' = k1 (LoseAt size2); n2' = k2 Win in sublate n1' n2'

ping :: Nom () (E Char)
ping = Nom start where
  start ~() = Yex (E (Just 'c')) 1 (waiting 1) where
    waiting _ Win = Nom start
    waiting _ Tie = Nom start
    waiting timer (LoseAt t) =
      let timer' = timer - t
      in Nom (\() -> Yex (E Nothing) timer' (waiting timer'))

term :: Nom (E Char) ()
term = Nom (const start) where
  start :: Yex (E Char) ()
  start = Yex () (1/0) k
  k :: WinLose -> Nom (E Char) ()
  k Win = error "end of time"
  k Tie = error "end of time"
  k (LoseAt t) =
    let us = floor (t * 1000000)
        !del = unsafePerformIO (threadDelay us)
    in Nom $ \i -> case i of
      E Nothing  -> start
      E (Just c) -> unsafePerformIO (putStrLn [c]) `seq` start


-- | Act like given guest 'Nom' until an event carrying a new guest arrives.
-- At the time of the event, there's no time for the old guest to react. It's replaced immediately
-- and processes the input it showed up with.
host :: Nom i o -> Nom (E (Nom i o), i) o
host (Nom guest) = Nom $ \(E mg, i) -> case mg of
  Nothing           -> let Yex o size k = guest  i in Yex o size (host . k)
  Just (Nom guest') -> let Yex o size k = guest' i in Yex o size (host . k)
  

-- | The wrapped 'Nom' that would not otherwise be disturbed is only interrupted by a new
-- input fragment if it passes a test. When the test fails the original output (already in progress)
-- is used, saving on some recomputation cost.
sentry :: Sym o => (i -> Bool) -> Nom i o -> Nom i o
sentry p (Nom nom) = Nom openMode where
  openMode i = let y@(Yex o size k) = nom i in Yex o size (closedMode y)
  closedMode (Yex _ _ oldK) Win = sentry p (oldK Win)
  closedMode (Yex _ _ oldK) Tie = sentry p (oldK Tie)
  closedMode (Yex oldOut oldSize oldK) (LoseAt t) = Nom $ \i -> if p i
    then
      let Nom nom' = oldK (LoseAt t)
          y@(Yex o size k) = nom' i
      in Yex o size (closedMode y)
    else
      let o = burn t oldOut
          size = oldSize - t
          k = oldK . (ageLosing t)
      in Yex o size (closedMode (Yex o size k))
  ageLosing dt Win = Win
  ageLosing dt Tie = Tie
  ageLosing dt (LoseAt t) = LoseAt (t + dt)
  


-- | A symbol contains enough information to reconstruct a finite part of a Behavior.
-- They also serve to describe the format of a fragment stream.
class Sym a where

  -- | Burn away an initial segment of a fragment.
  burn :: Time -> a -> a

  default burn :: (Generic a, Sym' (Rep a)) => Time -> a -> a
  burn t x = to (burn' t (from x))


-- | A product of symbols is used to represent a multi-channel signal.
instance (Sym a, Sym b) => Sym (a,b) where
  burn dt (x,y) = (burn dt x, burn dt y)

instance (Sym a, Sym b, Sym c) => Sym (a,b,c)
instance (Sym a, Sym b, Sym c, Sym d) => Sym (a,b,c,d)

    
-- | The function channel is interpreted as a non-varying function on symbols.
-- For Applicative convenience.
instance (Sym a, Sym b) => Sym (a -> b) where
  burn dt f = f

-- | Piecewise constant channel.
newtype K a = K a deriving (Eq,Ord,Show,Read)

-- | Event channel.
newtype E a = E (Maybe a) deriving (Eq,Ord,Show,Read)

-- | Varying value channel.
data V a = V !Time (Time -> a)

instance Show (V a) where
  showsPrec d (V off _) = showParen (d > 10) $ 
    ( showString "V "
    . shows off
    . showString " 📈" )

-- | Piecewise constant value in time.
instance Sym (K a) where
  burn _ (K x) = K x

-- | Events that occur at isolated points of time.
instance Sym (E a) where
  burn _ _ = E Nothing

-- | General time-varying value.
instance Sym (V a) where
  burn dt (V off f) = V (off + dt) f

-- | The unit symbol indicates steady @()@ forever in time.
instance Sym () where
  burn _ _ = ()


-- | Generic products, records of symbols can be used aggregate channels.
class Sym' f where
  burn' :: Time -> f p -> f p

instance (Sym' f) => Sym' (M1 i t f) where
  burn' t (M1 x) = M1 (burn' t x)

instance (Sym c) => Sym' (K1 i c) where
  burn' t (K1 x) = K1 (burn t x)

instance (Sym' f, Sym' g) => Sym' (f :*: g) where
  burn' t (x :*: y) = (burn' t x :*: burn' t y)
