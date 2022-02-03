{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Paint where

import Data.Functor.Const
import Data.Functor.Identity
import GHC.TypeLits
import Data.Proxy

import Data.Int

import Control.Monad.Reader

import HList
import Glue
import Common


import Unsafe.Coerce (unsafeCoerce) -- temporary

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
buildGate shader = fmap Gate . rMapWithNamesM (unsafeCoerce g) where
  g name (Frame setter) = do
    ul <- fmap UL $ getUniformLocation shader name
    return (Func (setter ul))

-- building blocks for buildGate
ug2f :: KnownSymbol n => RecF Frame ps -> RecF Frame ('(n,Float2) : ps)
ug2f next = (\(UL i) (F2 x y) -> setUniform2f i x y) `frame` next

ug3f :: KnownSymbol n => RecF Frame ps -> RecF Frame ('(n,Float3) : ps)
ug3f next = (\(UL i) (F3 x y z) -> setUniform3f i x y z) `frame` next

ug4f :: KnownSymbol n => RecF Frame ps -> RecF Frame ('(n,Float4) : ps)
ug4f next = (\(UL i) (F4 x y z w) -> setUniform4f i x y z w) `frame` next

capstone :: RecF Frame '[]
capstone = R0


-- want e.g. '[("foo",Float2),("bar",Float3)] to yield a function of type
-- Float2 -> Float3 -> Shader -> IO ()
-- which calls setUniform with the proper UL and correctly typed value.
-- we also want to get the ULs in the first place, using their string name.
-- (currently, we side stepped the getting UL directly by combining the getting with
-- the construction of the setter. maybe we can do it separately)

-- lets start with getting the ULs.

type Eg = ['("foo", Float2), '("bar", Float3)]

data UniLoc a = UniLoc Int32 (Int32 -> a -> IO ())

setUniforms :: RecF UniLoc ts -> Rec ts -> Shader -> IO ()
setUniforms R0 R0 _ = return ()
setUniforms (R1 (UniLoc i setter) is) (R1 (Identity x) xs) sh = do
  setter i x
  setUniforms is xs sh

class Uniforms ts where
  getLocs :: Shader -> IO (RecF UniLoc ts)
  getNames :: Proxy ts -> [String]

instance Uniforms '[] where
  getLocs _ = return R0
  getNames _ = []

instance (KnownSymbol name, Show t, UniType t, Uniforms ts) => Uniforms ('(name,t) ': ts) where
  getLocs sh = do
    let name = symbolVal (Proxy :: Proxy name)
    i <- getUniformLocation sh name
    is <- getLocs sh
    return (UniLoc i setUni `R1` is)

  getNames _ = symbolVal (Proxy :: Proxy name) : getNames (Proxy :: Proxy ts)
  

class UniType a where
  setUni :: Int32 -> a -> IO ()

instance UniType Float2 where
  setUni i (F2 x y) = setUniform2f i x y

instance UniType Float3 where
  setUni i (F3 x y z) = setUniform3f i x y z

instance UniType Float4 where
  setUni i (F4 x y z w) = setUniform4f i x y z w

data Sampler2D = Sampler2D Int32 deriving Show

instance UniType Sampler2D where
  setUni i (Sampler2D n) = setUniform1i i n




{-
class UniField t where
  name :: t -> String

instance UniField '(Symbol, x) where
  

class Uniforms ts where
  names :: ts -> [String]
  locations :: ts -> Shader -> IO [(String, Int)]

instance Uniforms (Rec []) where
  names _ = []

instance (Uniforms ts, UniField t) => Uniform (Rec (t ': ts)) where
  names (R1 x xs) = name x : names xs
  
  

-- we should be able to generate the gate blocks from the type itself, like the Printf class.
goal :: UniField ps -> 
-}



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


