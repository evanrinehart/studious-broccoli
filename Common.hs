{-# LANGUAGE DataKinds #-}
module Common where

import Data.Vector.Storable as V (Vector, fromList)
--import Graphics.GL

data Int2 = I2 !Int !Int deriving Show
data Float2 = F2 !Float !Float deriving Show
data Float3 = F3 !Float !Float !Float deriving Show
data Float4 = F4 !Float !Float !Float !Float deriving Show
data Rect a = Rect !a !a !a !a
type RGB = Float3

newtype Glyph = Glyph Int

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

i22f2 :: Int2 -> Float2
i22f2 (I2 x y) = F2 (fi x) (fi y)

ffloor :: Double -> Double
ffloor x = fi (floor x :: Int)

{-  a01   b11
 -  c00   d10  -}
tileData :: Vector Float
tileData =
  let a = [0, 1] in
  let b = [1, 1] in
  let c = [0, 0] in
  let d = [1, 0] in
  V.fromList (concat [c,d,b,b,a,c])


  --let box = 33 + 40 * 35 + 30 + 369
  --darkbox = 33 + 40 * 35 + 29

newtype Act = Act (IO ())

instance Monoid Act where
  mempty = Act $ return ()

instance Semigroup Act where
  Act a <> Act b = Act (a >> b)

onFst :: (a -> c) -> (a,b) -> (c,b)
onFst f (x,y) = (f x, y)
onSnd :: (b -> c) -> (a,b) -> (a,c)
onSnd f (x,y) = (x,f y)

deleteAt :: Int -> [a] -> [a]
deleteAt 0 (x:xs) = xs
deleteAt i (x:xs) = x : deleteAt (i-1) xs
deleteAt _ [] = error "index out of bounds"

insertAt :: Int -> a -> [a] -> [a]
insertAt 0 z (x:xs) = z : x : xs
insertAt i z (x:xs) = x : insertAt (i-1) z xs
insertAt 0 z [] = [z]
insertAt i z [] = error "index out of bounds"
