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
import Data.Proxy

import GHC.TypeLits

-- "extensible" records
data RecF :: (* -> *) -> [(Symbol,*)] -> * where
  R0 :: RecF f '[]
  R1 :: KnownSymbol s => f t -> RecF f ps -> RecF f ('(s,t) ': ps)

type Rec = RecF Identity

newtype Func b a = Func {appFunc :: a -> b}

rApply :: RecF (Func b) ps -> RecF Identity ps -> RecF (Const b) ps
rApply R0 R0 = R0
rApply (R1 (Func f) next1) (R1 (Identity x) next2) = R1 (Const (f x)) (rApply next1 next2)

rFold :: (forall a . f a -> b -> b) -> b -> RecF f ps -> b
rFold f base R0 = base
rFold f base (R1 x next) = f x (rFold f base next)

rMapWithNames :: forall f g ps . (forall a . String -> f a -> g a) -> RecF f ps -> RecF g ps
rMapWithNames visit R0 = R0
rMapWithNames visit ent@(R1 x next) = case ent of
  (_ :: KnownSymbol n => RecF f ('(n, t) : qs)) ->
    let name = symbolVal (Proxy :: Proxy n)
    in R1 (visit name x) (rMapWithNames visit next)

rMapWithNamesM :: forall f g ps . (forall a . String -> f a -> IO (g a)) -> RecF f ps -> IO (RecF g ps)
rMapWithNamesM visit R0 = return R0
rMapWithNamesM visit ent@(R1 x next) = case ent of
  (_ :: KnownSymbol n => RecF f ('(n, t) : qs)) -> do
    let name = symbolVal (Proxy :: Proxy n)
    y <- visit name x
    next' <- rMapWithNamesM visit next
    return (R1 y next')

-- helper to construct Recs
newtype Field (n::Symbol) a = Field a

infixr 5 >:
(>:) :: KnownSymbol n => Field n a -> Rec ps -> Rec ('(n,a) : ps)
(Field x) >: xs = Identity x `R1` xs





--- plain heterogeneous list

data HList :: (* -> *) -> [*] -> * where
  H0 :: HList f '[]
  H1 :: f t -> HList f ts -> HList f (t ': ts)

hApply :: HList (Func b) ts -> HList Identity ts -> HList (Const b) ts
hApply H0 H0 = H0
hApply (H1 (Func f) next1) (H1 (Identity x) next2) = H1 (Const (f x)) (hApply next1 next2)

hFold :: (forall a . f a -> b -> b) -> b -> HList f ts -> b
hFold f base H0 = base
hFold f base (H1 x next) = f x (hFold f base next)

