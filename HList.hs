{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module HList where

import Data.Functor.Const
import Data.Functor.Identity

import GHC.TypeLits

data HList :: (* -> *) -> [*] -> * where
  H0 :: HList f '[]
  H1 :: f t -> HList f ts -> HList f (t ': ts)

newtype Func b a = Func {appFunc :: a -> b}

hApply :: HList (Func b) ts -> HList Identity ts -> HList (Const b) ts
hApply H0 H0 = H0
hApply (H1 (Func f) next1) (H1 (Identity x) next2) =
  H1 (Const (f x)) (hApply next1 next2)

hFold :: (forall a . f a -> b -> b) -> b -> HList f ts -> b
hFold f base H0 = base
hFold f base (H1 x next) = f x (hFold f base next)

data Rec :: (* -> *) -> [Symbol] -> [*] -> * where
  R0 :: Rec f '[] '[]
  R1 :: KnownSymbol s => f t -> Rec f ss ts -> Rec f (s ': ss) (t ': ts)

rApply :: Rec (Func b) ns ts -> Rec Identity ns ts -> Rec (Const b) ns ts
rApply R0 R0 = R0
rApply (R1 (Func f) next1) (R1 (Identity x) next2) =
  R1 (Const (f x)) (rApply next1 next2)

rFold :: (forall a . f a -> b -> b) -> b -> Rec f ns ts -> b
rFold f base R0 = base
rFold f base (R1 x next) = f x (rFold f base next)
