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
-- at a time you otherwise would've done nothing. Place a sentry to isolate inner 'Nom's from
-- fragmentation when nothing of interest has happened, which should also improve performance.
data Nom i o = Nom { nomNom :: i -> i -> Yex i o } deriving Functor

-- | A chunk of output from a 'Nom' paired with a continuation. The losing continuation
-- is only valid for times less than yexSize.
data Yex i o = Yex
  { yexPrev :: o
  , yexOut  :: o
  , yexSize :: Time
  , yexNext :: WinLose -> Nom i o }
      deriving Functor

instance Show o => Show (Yex i o) where
  showsPrec d (Yex o0 o1 size _) = showParen (d > 10) $
    ( showString "Yex "
    . showsPrec 11 o0
    . showString " "
    . showsPrec 11 o1
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
  pure x = let go = Nom (\_ _ -> Yex x x (1/0) (const go) ) in go
  Nom ff <*> Nom xx = Nom (\i j -> ff i j <*> xx i j)

instance Applicative (Yex i) where
  pure x = Yex x x (1/0) (const (pure x))
  Yex f0 f1 size1 k1 <*> Yex x0 x1 size2 k2 =
    Yex (f0 x0) (f1 x1) (min size1 size2) (liftA2 (<*>) k1 k2)

-- | Lift (causal) functions on symbols to a stateless transducer.
instance Arrow Nom where
  arr f = let go = Nom (\i0 i1 -> Yex (f i0) (f i1) (1/0) (const go)) in go
  Nom f *** Nom g = Nom $ \(i0,j0) (i1,j1) ->
    let Yex o0 o1 size1 k1 = f i0 i1
        Yex p0 p1 size2 k2 = g j0 j1
    in Yex (o0,p0) (o1,p1) (min size1 size2) (liftA2 (***) k1 k2)


-- | Serial composition.
instance Category Nom where
  id = let go = Nom (\i0 i1 -> Yex i0 i1 (1/0) (const go)) in go
  Nom f . Nom g = Nom $ \i0 i1 ->
    let Yex z0 z1 size1 k1 = g i0 i1
        Yex o0 o1 size2 k2 = f z0 z1
    in Yex o0 o1 (min size1 size2) (liftA2 (.) k2 k1)

-- | Pre and post processing of chunks.
instance Profunctor Nom where
  lmap g (Nom nom) = Nom $ \i0 i1 -> let Yex o0 o1 s k = nom (g i0) (g i1) in Yex o0 o1 s (lmap g . k)
  rmap f (Nom nom) = Nom $ \i0 i1 -> let Yex o0 o1 s k = nom i0 i1 in Yex (f o0) (f o1) s (fmap f . k)

-- | A timed sequence of symbolic fragments. The time indicates the fragment size.
-- The symbol encodes a continuum of values during the fragment time.
--
-- An infinite fragment at the end of the sequence or an infinite sequence of finite fragments
-- are both acceptable.
--
-- When @c@ is an instance of 'Sym', a @Tape c@ can be interpreted as a continually varying
-- haskell value slice, aka a behavior. The type of the slice depends on @c@.
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






-- | A symbol contains enough information to reconstruct a finite part of a behavior.
-- They also serve to describe the format of a fragment stream.
class Sym a where

  -- | Burn away an initial segment of a fragment.
  burn :: Time -> a -> a

  default burn :: (Generic a, Sym' (Rep a)) => Time -> a -> a
  burn t x = to (burn' t (from x))

-- | Generic products, records of symbols can be used aggregate channels.
class Sym' f where
  burn' :: Time -> f p -> f p

instance (Sym' f) => Sym' (M1 i t f) where
  burn' t (M1 x) = M1 (burn' t x)

instance (Sym c) => Sym' (K1 i c) where
  burn' t (K1 x) = K1 (burn t x)

instance (Sym' f, Sym' g) => Sym' (f :*: g) where
  burn' t (x :*: y) = (burn' t x :*: burn' t y)

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




-- | Consider a 'Nom'@ i o@ to be a causal transducer of 'Tape's.
transduce :: (Sym i, Sym o) => Nom i o -> Tape i -> Tape o
transduce n (Tape xs) = Tape (go undefined n xs) where
  go i0 (Nom _) [] = []
  go i0 (Nom nom) ((i1,isize):more) =
    let Yex _ o osize k = nom i0 i1 in
    case compare osize isize of
      LT -> let i1' = burn osize i1 in (o,osize) : go i1' (k Win) ((i1', isize - osize):more)
      EQ -> let i1' = burn osize i1 in (o,osize) : go i1' (k Tie) more
      GT -> let i1' = burn isize i1 in (o,isize) : go i1' (k (LoseAt isize)) more

-- | @memory x@ outputs @K x@ until an event arrives with an update.
memory :: a -> Nom (E (a -> a)) (K a)
memory x = Nom $ \_ i1 -> case i1 of
  E Nothing  -> Yex (K x) (K x) (1/0) (const (memory x))
  E (Just f) -> let x' = f x in x' `seq` Yex (K x) (K x') (1/0) (const (memory x'))


-- | Let @t@ be the time of the first occurrence of the @E@ carrying @m@ (new @Nom@). Then
--
-- @
-- ⟦host n⟧ xs = take t (⟦n⟧ (fmap snd xs)) ++ ⟦host m⟧ (drop t xs)
-- @
--
-- Dynamically switch to a new 'Nom'. The old one is lost immediately and has no time to react.
host :: Nom i o -> Nom (E (Nom i o), i) o
host (Nom guest) = Nom $ \(_, i0) (E mg, i1) -> case mg of
  Nothing           -> let Yex o0 o1 size k = guest i0 i1 in Yex o0 o1 size (host . k)
  Just (Nom guest') ->
    let Yex p0 p1 size k = guest' i0 i1
    in Yex p0 p1 size (host . k)

  
-- | Feedback composition. There is infinitesimal delay on the 2nd input channel to guard against
-- divergence of outputs that depend on themselves.
feedback :: Nom (i,o) o -> Nom i o
feedback (Nom nom) = Nom $ \i0 i1 ->
  let Yex o0 o1 size k = nom (i0,o0) (i1,o0 {-!-}) in Yex o0 o1 size (feedback . k)

{--

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

(☯) :: Nom i o -> Nom o i -> a
(!n1) ☯ (!n2) = (k1 r1) ☯ (k2 r2) where
  Yex o !size1 k1 = n1 `nomNom` i
  Yex i !size2 k2 = n2 `nomNom` o
  (r1, r2) = case compare size1 size2 of
    LT -> (Win, LoseAt size1)
    EQ -> (Tie, Tie)
    GT -> (LoseAt size2, Win)



--}
