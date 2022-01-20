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
import Data.List.NonEmpty
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

-- | A particular kind of stream transducer. Also an arrow in a category where objects are
-- chunk formats and morphisms are causal functions between chunks with that format.
data Nom i o = Nom { stepNom :: i -> Yex i o } deriving Functor

-- | A chunk of output from a Nom paired with a resume continuation. The continuation
-- is only valid for times 0 < t <= yexSize.
data Yex i o = Yex
  { yexOut    :: o
  , yexSize   :: Time
  , yexResume :: Time -> Nom i o }
      deriving Functor

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
