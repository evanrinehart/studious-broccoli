{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Component where

import Control.Applicative
import Control.Comonad
import Control.Category
import Control.Arrow
import Data.Profunctor
import Data.Bifunctor
import Data.Biapplicative
import Data.Foldable
import Prelude hiding (splitAt, id, (.))
import Text.Printf
import Numeric

import TimeData

-- complete the rectangle of length l > 0
-- l = l2 + l1 => ext f l = ext f l2 . ext f l1
newtype F s a b = F { unF :: Time -> a -> s -> (b,s) } -- Category, Profunctor, ...

deriving instance Functor (F s a)

-- in f . g . h . i = f . (g . (h . i)), i "happens first"
-- but due to laziness we see the output of f first, which contains thunks.
-- to observe any effect of the arg, we must chew through all the layers.
-- effect such as "mouse outside box, do nothing"
instance Category (F s) where
  id = F (\_ a s -> (a,s))
  F gen2 . F gen1 = F (\l a s -> let (b,s') = gen1 l a s in gen2 l b s')

instance Semigroup s => Arrow (F s) where
  arr f = F (\_ a s -> (f a,s))
  F gen1 *** F gen2 = F gen3 where
    gen3 l (a1,a2) s = ((b1,b2), s1' <> s2') where
      (b1, s1') = gen1 l a1 s
      (b2, s2') = gen2 l a2 s

instance Profunctor (F s) where
  rmap = fmap
  lmap g (F gen) = F (\l z s -> gen l (g z) s)

instance (Semigroup s, Semigroup b) => Semigroup (F s a b) where
  F gen1 <> F gen2 = F gen3 where
    gen3 l a s = (b1 <> b2, s1 <> s2) where
      (b1,s1) = gen1 l a s
      (b2,s2) = gen2 l a s

-- | Bundle 'F' with a state and hide the type.
data Guest a b = forall s . Guest s (F s a b)
deriving instance Functor (Guest a)

instance Profunctor Guest where
  rmap = fmap
  lmap g (Guest s bigF) = Guest s (lmap g bigF)

feedGuest :: Time -> a -> Guest a b -> (b, Guest a b)
feedGuest l a (Guest s (F gen)) = let (b,s') = gen l a s in (b, Guest s' (F gen))

-- | There is a free 'F' for any 'Guest'
wrappedGuest :: F (Guest a b) a b
wrappedGuest = F (\l a (Guest s (F gen)) -> let (b,s') = gen l a s in (b, Guest s' (F gen)))



-- | Similar to 'F' but the state type can change. This allows two functions to collect
-- the outputs and new state from a hetero-typed \"hand\" of components, using '(<<*>>)'.
newtype M a p b q = M { unM :: Time -> a -> p -> (b, q) }

-- | By temporarily rearranging the letters you can 'lmap' and 'rmap'.
newtype M' p q a b = M' { unM' :: M a p b q }

instance Bifunctor (M a p) where
  bimap f g (M gen) = M (\l a s -> let (b,s') = gen l a s in (f b, g s'))

instance Biapplicative (M a p) where
  bipure b u = M (\_ _ _ -> (b,u))
  M f <<*>> M g = M h where
    h l a s = (bf bx, uf ux) where
      (bf, uf) = f l a s
      (bx, ux) = g l a s

instance Profunctor (M' p q) where
  lmap f (M' (M gen)) = M' (M (\l a s -> gen l (f a) s ))
  rmap f (M' (M gen)) = M' (M (\l a s -> let (b,s') = gen l a s in (f b, s')))

instance Functor (M' p q a) where
  fmap = rmap

fToM :: F s a b -> M a s b s
fToM (F gen) = M gen
mToF :: M a s b s -> F s a b
mToF (M gen) = F gen

-- | Set up a component with a slice of state in preparation for '<<*>>'.
wrap :: (s -> u) -> F u a b -> M a s b u
wrap field (F gen) = M (\l a -> gen l a . field)

-- | When all components are gathered and the full state is reassembled,
-- loop output back to input. This lets components have a more or less limited
-- view of each other.
feedback :: M (a,b) s b s -> F s a b
feedback (M gen) = F (\l a s -> let (b,s') = gen l (a,b) s in (b,s'))
