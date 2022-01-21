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

-- | A morphism in a category where objects are timed sequences of 'Symbol's
-- and arrows are causal transducers.
--
-- @[[Nom i o]] = Frags i -> Frags o (causal)@
--
-- Here @i@ and @o@ are instances of 'Symbol', though it's not enforced everywhere.
data Nom i o = Nom { stepNom :: i -> Yex i o } deriving Functor

-- | A chunk of output from a Nom paired with a continuation. The losing continuation
-- is only valid for times less than yexSize.
data Yex i o = Yex
  { yexOut  :: o
  , yexSize :: Time
  , yexNext :: WinLose -> Nom i o }
      deriving Functor

data WinLose = Win | LoseAt Time | Tie deriving (Eq,Ord,Show,Read)

-- | Parallel composition.
instance Applicative (Nom i) where
  pure x = let go = Nom (\_ -> Yex x (1/0) (const go) ) in go
  Nom ff <*> Nom xx = Nom (liftA2 (<*>) ff xx)

instance Applicative (Yex i) where
  pure x = Yex x (1/0) (const (pure x))
  Yex f size1 k1 <*> Yex x size2 k2 = Yex (f x) (min size1 size2) (liftA2 (<*>) k1 k2)

-- | Lift (causal) functions on chunks to a stateless Nom
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

newtype Frags a = Frags [(a,Time)] deriving (Show,Functor)

class Symbol a => Burn a where
  burn :: Time -> a -> a

-- | Run a Nom over some input chunks. The last chunk, if it exists, may be infinite.
-- It takes input in format i and produces output in format o.
transduce :: Burn i => Nom i o -> Frags i -> Frags o
transduce n (Frags xs) = Frags (go n xs) where
  go (Nom _) [] = []
  go (Nom nom) ((i,isize):more) =
    let Yex o osize k = nom i in
    case compare osize isize of
      LT -> (o,osize) : go (k Win) ((burn osize i, isize - osize):more)
      EQ -> (o,osize) : go (k Tie) more
      GT -> (o,isize) : go (k (LoseAt isize)) more

-- | Feedback composition. The output chunk is immediately available on 2nd input channel.
-- The resulting Nom may be more or less undefined.
feedback :: Nom (i,o) o -> Nom i o
feedback (Nom nom) = Nom (\i -> let Yex o size k = nom (i,o) in Yex o size (feedback . k))


-- | A symbol contains enough information to reconstruct a finite part of a Behavior.
-- They also serve to describe the format of a fragment stream.
-- (Name conflict with GHC type level strings also called Symbol.)
class Symbol a where
  type Slice a :: *
  sampleAt :: Time -> a -> Slice a
  frozen :: Slice a -> a



instance (Symbol a, Symbol b) => Symbol (a,b) where
  type Slice (a,b) = (Slice a, Slice b)
  sampleAt t (x,y) = (sampleAt t x, sampleAt t y)
    
-- | The function channel is interpreted as a frag-constant function on encoded fragments.
-- Allows Applicative to make sense.
instance (Symbol a, Symbol b) => Symbol (a -> b) where
  type Slice (a -> b) = Slice a -> Slice b
  sampleAt t g x = let x' = frozen x in sampleAt 0 (g x')
  

-- | Piecewise constant value channel.
newtype K a = K a

-- | Event channel.
newtype E a = E (Maybe a)

-- | Varying value channel.
data V a = V Time (Time -> a)

instance Symbol (K a) where
  type Slice (K a) = a
  sampleAt _ (K x) = x
  frozen x = K x

instance Symbol (E a) where
  type Slice (E a) = Maybe a
  sampleAt t (E mx) = if t > 0 then Nothing else mx
  frozen mx = E mx

instance Symbol (V a) where
  type Slice (V a) = a
  sampleAt t (V off f) = f (t + off)
  -- decode = sampleAt (not zero)
  frozen x = V 0 (const x)

instance Symbol () where
  type Slice () = ()
  sampleAt _ _ = ()
  -- decode = sampleAt  anywhere
  frozen _ = ()




-- | A Behavior is a time-varying value.
class Behavior behavior where
  at       :: behavior a -> Time -> a
  embedFun :: (Time -> a) -> behavior a

instance Behavior ((->) Time) where
  at       = id
  embedFun = id

-- | Glue all the fragments together to get a Behavior. The inverse of 'shatter'.
heal :: (Symbol a, Behavior b) => Frags a -> b (Slice a)
heal (Frags xs) = embedFun $ \t ->
  let go ptr ((sym,l):more) =
        let ptr' = ptr + l in
        if t < ptr'
          then sampleAt (t - ptr) sym
          else go ptr' more
      go _ [] = error "ran out of time"
  in go 0 xs

-- | A behavior that can be reduced to a sequence of symbols in different ways.
class Behavior b => Symbolizable b where
  symbolizeAt :: Symbol a => b (Slice a) -> Time -> (a, Time)

-- | (currently broken) Break up a behavior into symbolic fragments. Regardless of
-- the chosen fragmentation 'heal' returns the original behavior.
shatter :: (Symbol a, Symbolizable b) => [Time] -> b (Slice a) -> Frags a
shatter (base:ts) b = Frags (go base ts) where
  go t more = 
    let (x, size) = symbolizeAt b t in
    case more of
      [] -> [(x, size)]
      (t':more') -> go t' more'

-- What is a Behavior? I.e. Frags (V a) ==> Time -> a
-- What is a Behavior? I.e. Frags (E a) ==> [(Time, a)]
-- What is a Behavior? I.e. Frags (K a) ==> [(a, Time)]
-- What is a Behavior? I.e. Frags (V a, E a) ==> (Time -> a, [(Time, a)]) ?
-- What is a Behavior? I.e. Frags (V a -> E a) ==> [(V a -> E a, Time)] (mostly uninterpreted)

-- a Behavior indexed by symbol a is a function Time -> Slice a
-- where Slice describes what you would see if you sampled the function.
-- Slice (K a) = a
-- Slice (E a) = Maybe a
-- Slice (V a) = a
-- Slice (s1,s2) = (Slice s1, Slice s2)
-- Slice (s1 -> s2) = s1 -> s2 -- ??

-- is Behavior Applicative?
-- B (u -> v) -> B u -> B v ?
-- (Time -> u -> v) -> (Time -> Slice u) -> Time -> Slice v ?
-- "proof" \ff fu t -> ff t
