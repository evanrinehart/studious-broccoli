module Grid where

import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap, (!))

import Common

type Grid a = IntMap (IntMap a)

gridLookup :: Int -> Int -> Grid a -> Maybe a
gridLookup i j grid = IM.lookup j grid >>= IM.lookup i

foldlGrid :: (Int -> Int -> a -> b -> b) -> b -> Grid a -> b
foldlGrid visit start rows = IM.foldlWithKey' f start rows where
  f acc j row = IM.foldlWithKey' g acc row where
    g acc i x = visit i j x acc

foldrGrid :: (Int -> Int -> a -> b -> b) -> b -> Grid a -> b
foldrGrid visit start rows = IM.foldrWithKey f start rows where
  f j row acc = IM.foldrWithKey g acc row where
    g i x acc = visit i j x acc

gridAssoc :: Grid a -> [(Int,Int,a)]
gridAssoc = foldrGrid g [] where
  g i j x trips = (i,j,x):trips

gridMod :: (a -> a) -> Int -> Int -> Grid a -> Grid a
gridMod f i j = IM.adjust (IM.adjust f i) j

gridInsert :: Int -> Int -> a -> Grid a -> Grid a
gridInsert i j x grid = IM.alter g j grid where
  g (Just row) = Just $ IM.insert i x row
  g Nothing    = Just $ IM.singleton i x

gridUpsert :: Int -> Int -> (a -> a) -> a -> Grid a -> Grid a
gridUpsert i j f d grid = IM.alter g j grid where
  g mrow = case mrow of
    Nothing  -> Just (IM.singleton i d)
    Just row -> Just (IM.alter h i row) where
      h (Just x) = Just (f x)
      h Nothing  = Just d 

gridFromList :: [((Int,Int), a)] -> Grid a
gridFromList ps = foldl (\grid ((i,j),x) -> gridInsert i j x grid) IM.empty ps

gridSingleton :: Int -> Int -> a -> Grid a
gridSingleton i j x = IM.singleton j (IM.singleton i x)

gridToList :: Grid a -> [((Int,Int),a)]
gridToList grid = concatMap (\(j,row) -> map (\(i,x) -> ((i,j),x)) (IM.toList row)) (IM.toList grid)



radiusToArea :: Float2 -> Float -> Float4
radiusToArea c r = let F2 x y = c - F2 r r in F4 x y (2*r) (2*r)

overlapping :: Float4 -> Float4 -> Bool
overlapping (F4 x1 y1 w1 h1) (F4 x2 y2 w2 h2) = not apart where
  apart = x2 > x1 + w1 || x1 > x2 + h2 || y2 > y1 + h1 || y1 > y2 + h2

overlappingIntBox :: Int4 -> Int4 -> Bool
overlappingIntBox (I4 x1 y1 w1 h1) (I4 x2 y2 w2 h2) = not apart where
  apart = x2 > x1 + w1 || x1 > x2 + h2 || y2 > y1 + h1 || y1 > y2 + h2
