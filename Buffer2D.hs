module Buffer2D where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

data Rect a = Rect a a a a

type Buffer2D a = (a, IntMap (IntMap a))
blank :: a -> Buffer2D a
getItem :: Buffer2D a -> Int -> Int -> a
putItem :: a -> Int -> Int  -> Buffer2D a -> Buffer2D a
clearItem :: Int -> Int -> Buffer2D a -> Buffer2D a
--getItems :: a -> Buffer2D a -> Int -> Int -> Int -> [a]
--putItems :: Int -> Int -> [a] -> Buffer2D a -> Buffer2D a
--mapBuffer :: Buffer2D a -> Rect Int -> (Int -> Int -> a -> b) -> [[b]]

blank x = (x, IM.empty)

getItem (d,buf) i j = case IM.lookup j buf of
  Just row -> IM.findWithDefault d i row
  Nothing  -> d

putItem x i j (d,buf) = (d, IM.alter f j buf) where
  f Nothing    = Just (IM.singleton i x)
  f (Just row) = Just (IM.insert i x row)

clearItem i j (d,buf) = (d, IM.alter f j buf) where
  f Nothing    = Nothing
  f (Just row) =
    let row' = IM.delete i row in
    if null row' then Nothing else Just row'

foldBuffer :: Rect Int -> (Int -> Int -> a -> b -> b) -> b -> Buffer2D a -> b
foldBuffer (Rect wi wj ww wh) f base (d,buf) = foldSlice IM.empty buf wj (wj+wh-1) g base where
  g j row accum = foldSlice d row wi (wi+ww-1) (\i x acc -> f i j x acc) accum
  
foldSlice :: a -> IntMap a -> Int -> Int -> (Int -> a -> b -> b) -> b -> b
foldSlice d im a b f base = go a base where
  go i accum | i <= b    = f i (IM.findWithDefault d i im) (go (i+1) accum)
             | otherwise = accum

--mapBuffer (d,buf) (Rect bi bj w h) f = map g (viewSlice IM.empty buf bj (bj+h-1)) where
  --g (j,row) = map (\(i,x) -> f i j x) (viewSlice d row bi (bi+w-1))
  --viewSlice :: a -> IntMap a -> Int -> Int -> [(Int,a)]
  --viewSlice d im a b = map f [a..b] where
    --f i = (i, IM.findWithDefault d i im)

