{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Paint where

import Data.Functor.Const
import Data.Functor.Identity
import GHC.TypeLits
import Data.Proxy

import Control.Monad.Reader

import Glue
import Common

import HList

data Paint u = Paint VAO Shader u


{-
newtype Key ts  = Key  (HList Identity ts)
newtype Gate ts = Gate (HList Eater ts)

unlock :: Gate ts -> Key ts -> IO ()
unlock (Gate g) (Key k) =
  let noop = return ()
  in hFold (\(Const io) io' -> io >> io') noop (hApply g k)

configPaint :: HList Identity ts -> Paint ts -> IO ()
configPaint xs (Paint _ _ gate) = unlock gate (Key xs)

key :: t -> HList Identity ts -> HList Identity (t ': ts)
key x next = Identity x `H1` next

buildGate :: (t -> IO ()) -> HList Eater ts -> HList Eater (t ': ts)
buildGate action next = H1 (Func action) next

bg2f :: String -> HList Eater ts -> ReaderT Shader IO (HList Eater (Float2 ': ts))
bg2f name next = do
  shader <- ask
  ul <- liftIO $ getUniformLocation shader name
  return $ Func (\(F2 x y) -> setUniform2f ul x y) `H1` next
-}

{-
bg3f name next = do
  shader <- ask
  ul <- getUniformLocation shader name
  return $ buildGate (\(F3 x y z) -> setUniform3f ul x y z) next
-}

newtype Gate ns ts = Gate (Rec (Func (IO ())) ns ts)
newtype Key ns ts = Key (Rec Identity ns ts)

type G1 = Gate
  ["winWH", "srcXY", "srcWH", "dstXY", "dstWH"]
  [Float2,   Float2,  Float2,  Float2,  Float2]

ex :: Shader -> IO G1
ex shader = 
  let capstone = Gate R0 in
  bgr2f shader capstone >>= bgr2f shader >>= bgr2f shader >>= bgr2f shader >>= bgr2f shader

bgr2f :: forall n ns ts . KnownSymbol n => Shader -> Gate ns ts -> IO (Gate (n : ns) (Float2 : ts))
bgr2f shader (Gate next) = do
  let name = symbolVal (Proxy :: Proxy n)
  ul <- getUniformLocation shader name
  (return . Gate) (Func (\(F2 x y) -> setUniform2f ul x y) `R1` next)

bgr3f :: forall n ns ts . KnownSymbol n => Shader -> Gate ns ts -> IO (Gate (n : ns) (Float3 : ts))
bgr3f shader (Gate next) = do
  let name = symbolVal (Proxy :: Proxy n)
  ul <- getUniformLocation shader name
  (return . Gate) (Func (\(F3 x y z) -> setUniform3f ul x y z) `R1` next)
  
key :: forall n t ns ts . KnownSymbol n => t -> Key ns ts -> Key (n : ns) (t : ts)
key x (Key next) = Key (Identity x `R1` next)

unlock :: Gate ns ts -> Key ns ts -> IO ()
unlock (Gate g) (Key k) = 
  let noop = return ()
  in rFold (\(Const io) io' -> io >> io') noop (rApply g k)


