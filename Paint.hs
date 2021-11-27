{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Paint where

import Data.Functor.Const
import Data.Functor.Identity
import GHC.TypeLits
import Data.Proxy

import Control.Monad.Reader

import HList
import Glue
import Common

-- A combination of VAO, shader, and uniform locations.
-- The uniform locations takes the form of an IO action
-- that requires a Rec of new uniform values to set.

-- use buildGate to create a new gate
-- use >: to build keys
-- type synonyms to describe the uniforms are like type P1 = [("var1",Float2)]
data Paint rs = Paint VAO Shader (Gate rs)

-- set the uniforms associated with the paint
configUniforms :: Gate rs -> Rec rs -> IO ()
configUniforms = unlock

-- initialize paint by getting uniform locations
buildGate :: Shader -> RecF Frame ps -> IO (Gate ps)
buildGate shader = fmap Gate . rMapWithNamesM g where
  g name (Frame setter) = do
    ul <- fmap UL $ getUniformLocation shader name
    return (Func (setter ul))

-- building blocks for buildGate
ug2f :: KnownSymbol n => RecF Frame ps -> RecF Frame ('(n,Float2) : ps)
ug2f next = (\(UL i) (F2 x y) -> setUniform2f i x y) `frame` next

ug3f :: KnownSymbol n => RecF Frame ps -> RecF Frame ('(n,Float3) : ps)
ug3f next = (\(UL i) (F3 x y z) -> setUniform3f i x y z) `frame` next

capstone :: RecF Frame '[]
capstone = R0


-- internal stuff
type Eater = Func (IO ())
newtype Frame a = Frame (UL -> a -> IO ())
newtype Gate rs = Gate (RecF Eater rs)

frame :: KnownSymbol n => (UL -> a -> IO ()) -> RecF Frame ps -> RecF Frame ('(n,a) : ps)
frame act next = R1 (Frame act) next

unlock :: Gate rs -> Rec rs -> IO ()
unlock (Gate g) k = 
  let noop = return ()
  in rFold (\(Const io) io' -> io >> io') noop (rApply g k)



-- test examples
{-
ex :: Shader -> IO (Gate UL1)
ex sh = buildGate sh (stones capstone) where
  stones = ug2f . ug2f . ug2f . ug2f . ug2f
-}
type L = ['("numberA", Int), '("numberB", Int), '("numberC", Int)]
{- ... -}
numbers :: Rec L
numbers =
  Field @"numberA" 3 >:
  Field @"numberB" 4 >:
  Field @"numberC" 999 >:
  R0

