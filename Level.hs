{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
module Level where

import Data.Map (Map); import qualified Data.Map as M
import Control.Monad.Reader
import Debug.Trace

import Data.Reflection

import Common
import Grid
import Geometry

data AbstractLevel = AL Int Int (Grid Char) deriving Show
data Plat = Air | Wall | Pad | Block Int Int deriving Show
data Material = O | M deriving Show

data LenCode  = Long | Short | ShortPlusL | ShortPlusR deriving (Eq,Ord,Show)
data ElemCode = OutCorner !Int !Int | Edge !Int !Int !LenCode !Int deriving (Eq,Ord,Show)
data MicroCode = Site !Int !Int !Int !Micro deriving (Eq,Ord,Show)
data Nine a = Nine a a a a a a a a a deriving (Functor,Show)
data Ni a = Ni a a a deriving Show
data Pent a = Pent a a a a a deriving Show
data Quad a = Quad a a a a deriving Show
type Build a = [a] -> [a]





-- AbstractLevel is the most abstract form of a level and can be loaded from a file
-- Plat indicates what sort of object the ball could hit
-- ElemCode is an abstract form of a geometric element that the ball could cross
-- MicroCode. The location of a microsite and the character of the microsurface (Micro) there

north = F2 0 1
south = -(F2 0 1)
east = F2 1 0
west = -(F2 1 0)
uu = Flat north
ud = Flat south
ul = Flat west
ur = Flat east
uur = Corner north east
uul = Corner north west
udr = Corner south east
udl = Corner south west

{-
visitSquare :: Int -> Int -> Grid c -> Map ElemCode Plat -> Map ElemCode Plat
visitSquare i j grid m = 
  let nine = gridWindow i j grid
      M.foldl' (\m (k,v) -> M.insert k v m) m (genSquare i j nine [])
-}


-- AL -> X -> V -> [At (Micro,[Plat])]
-- AL -> X -> R -> [(Elem,Plat)]
-- to turn world coordinates into grid indexes we need:
--   board dimensions
-- to turn element and micro codes into geometry, we need:
--   board dimensions
--   ball radius

-- to turn pad position and level into geometry we need:
--   ???

{-
foo :: WorldDimensions -> AbstractLevel -> Float2 -> Float -> [(ElemCode,Plat)]
foo wd al x r =
  let (box0,box1) = radiusToBox x r
      i1 = mapXYToIJ wd box0
      i2 = mapXYToIJ wd box1
      ixs = boundingBoxI2 i1 i2
      codes = genElemCodes al ixs
  in M.toList codes
-}

data WorldDimensions = WD
  { ballRadius :: Float
  , boardWH :: Float2
  , blockWH :: Float2 }
      deriving Show

--  1    0 is in center of canvas
-- 0 2   x increases right
--  3    y increases up
worldLine :: WorldDimensions -> LenCode -> Int -> (Float2, Float2)
worldLine (WD r _ (F2 bw bh)) len 0 =
  let (kl,kr) = adjustments r len
      y0 = 0 + kl
      y1 = bh - kr
  in (F2 r y0, F2 r y1)
worldLine (WD r _ (F2 bw bh)) len 1 =
  let (kl,kr) = adjustments r len
      x0 = 0 + kl
      x1 = bw - kr
      y  = bh - r
  in (F2 x0 y, F2 x1 y)
worldLine (WD r _ (F2 bw bh)) len 2 =
  let (kl,kr) = adjustments r len
      y0 = bh - kl
      y1 = 0 + kr
      x  = bw - r
  in (F2 x y0, F2 x y1)
worldLine (WD r _ (F2 bw bh)) len 3 =
  let (kl,kr) = adjustments r len
      x0 = bw - kl
      x1 = 0 + kr
  in (F2 x0 r, F2 x1 r)

adjustments :: Float -> LenCode -> (Float,Float)
adjustments r Long = (0,0)
adjustments r Short = (r,r)
adjustments r ShortPlusL = (0,r)
adjustments r ShortPlusR = (r,0)


--  1
-- 0 2
--  3
--data ElemCode = OutCorner !Int !Int | Edge !Int !Int !LenCode !Int deriving (Eq,Ord,Show)
concretizeElemCode :: WorldDimensions -> ElemCode -> Elem
concretizeElemCode wd@(WD r (F2 w h) (F2 bw bh)) code = case code of
  Edge i j len pos -> 
    let o = F2 (fi i * bw - w/2) (h/2 - fi (j+1) * bh)
        (a,b) = worldLine wd len pos
    in RuleAB (o + a) (o + b)
  OutCorner i j ->
    let cx = fi i * bw - w/2
        cy = h/2 - fi j * bh
    in Circle (F2 cx cy) r

cool 0 = (-1)
cool 1 = 0
cool 2 = 1
cool 3 = 1
cool 4 = 1
cool 5 = 0
cool 6 = (-1)
cool 7 = (-1)

sick 0 = 1
sick 1 = 1
sick 2 = 1
sick 3 = 0
sick 4 = (-1)
sick 5 = (-1)
sick 6 = (-1)
sick 7 = 0

concretizeMicro :: WorldDimensions -> MicroCode -> (Float2,Micro)
concretizeMicro (WD r (F2 w h) (F2 bw bh)) (Site i j position u) =
  let x = fi i * bw - w/2
      y = h/2 - fi j * bh
      shiftX = cool position * r
      shiftY = sick position * r
  in (F2 (x+shiftX) (y+shiftY), u)


boundingBoxI2 :: Int2 -> Int2 -> [Int2]
boundingBoxI2 (I2 a b) (I2 c d) = [I2 x y | x <- [xmin .. xmax], y <- [ymin .. ymax]] where
  xmin = min a c
  xmax = max a c
  ymin = min b d
  ymax = max b d

radiusToBox :: Float2 -> Float -> (Float2,Float2)
radiusToBox (F2 cx cy) r = (F2 (cx - r) (cy - r), F2 (cx + r) (cy + r))

rayToCorners :: WorldDimensions -> Float2 -> Float2 -> [Int2]
rayToCorners (WD _ (F2 w h) _) x v = [I2 i j | i <- [0..11], j <- [0..16]]
--rayToCorners (WD _ (F2 w h) _) x v = []

radiusToIndices :: WorldDimensions -> Float2 -> Float -> [Int2]
radiusToIndices wd x r =
  let (p1,p2) = radiusToBox x r
      ix1 = mapXYToIJ wd p1
      ix2 = mapXYToIJ wd p2
  in boundingBoxI2 ix1 ix2


-- map to the square index you're in
-- don't use this when you're close to a square edge
mapXYToIJ :: WorldDimensions -> Float2 -> Int2
mapXYToIJ (WD r (F2 w h) (F2 bw bh)) (F2 x y) = I2 i j where
  i = floor $ (x + w/2) / bw
  j = floor $ (h/2 - y) / bh

-- map to the cross index you're near
-- don't use this when you're close to middle of a square
mapXYToIJ' :: WorldDimensions -> Float2 -> Int2
mapXYToIJ' (WD r (F2 w h) (F2 bw bh)) (F2 x y) = I2 i j where
  i = floor $ ((x + w/2) / bw) + 0.5
  j = floor $ ((h/2 - y) / bh) + 0.5

platToMat Air = O
platToMat _   = M

lvlIndex :: Int -> Int -> AbstractLevel -> Plat
lvlIndex i j (AL w h blocks) = if j < 0 || i < 0 || i >= w
  then Wall
  else case gridLookup i j blocks of Just _ -> Block i j; Nothing -> Air

window9 :: Int2 -> AbstractLevel -> Nine Plat
window9 (I2 i j) lvl = Nine a0 a1 a2 b0 b1 b2 c0 c1 c2 where
  a0 = lvlIndex (i-1) (j-1) lvl
  a1 = lvlIndex (i-0) (j-1) lvl
  a2 = lvlIndex (i+1) (j-1) lvl
  b0 = lvlIndex (i-1) (j-0) lvl
  b1 = lvlIndex (i-0) (j-0) lvl
  b2 = lvlIndex (i+1) (j-0) lvl
  c0 = lvlIndex (i-1) (j+1) lvl
  c1 = lvlIndex (i-0) (j+1) lvl
  c2 = lvlIndex (i+1) (j+1) lvl

window4 :: Int2 -> AbstractLevel -> Quad Plat
window4 (I2 i j) lvl = Quad a b c d where
  a = lvlIndex (i-1) (j-1) lvl
  b = lvlIndex (i-0) (j-1) lvl
  c = lvlIndex (i-1) (j-0) lvl
  d = lvlIndex (i-0) (j-0) lvl

genElemCodes :: AbstractLevel -> [Int2] -> Map ElemCode Plat
genElemCodes lvl ijs = M.fromList $ foldr (.) id (map (\ij -> genSquare ij (window9 ij lvl)) ijs) []

genMicroCodes :: AbstractLevel -> [Int2] -> Map MicroCode [Plat]
genMicroCodes lvl ijs = M.fromList $ foldr (.) id (map (\ij -> genMicros ij (window4 ij lvl)) ijs) []

--  1
-- 0 2
--  3
genSquare :: Int2 -> Nine Plat -> Build (ElemCode,Plat)
genSquare (I2 si sj) n@(Nine x0 x1 x2 y0 y1 y2 z0 z1 z2) rest = 
  let Nine t0 t1 t2 m0 m1 m2 b0 b1 b2 = fmap platToMat n
      go = genCorner si sj         (Ni y0 x0 x1) .
           genCorner (si+1) sj     (Ni x1 x2 y2) .
           genCorner si (sj+1)     (Ni y0 z0 z1) .
           genCorner (si+1) (sj+1) (Ni y2 z2 z1) .
           genEdge si sj 0 y0 (Pent b1 b0 m0 t0 t1) .
           genEdge si sj 2 y2 (Pent t1 t2 m2 b2 b1) .
           genEdge si sj 1 x1 (Pent m0 t0 t1 t2 m2) .
           genEdge si sj 3 z1 (Pent m2 b2 b1 b0 m0)
  in case y1 of
    Air -> go rest
    _   -> rest
        

genCorner :: Int -> Int -> Ni Plat -> Build (ElemCode,Plat)
genCorner ci cj (Ni Air Air Air) rest = rest
genCorner ci cj (Ni a   Air Air) rest = (OutCorner ci cj, a) : rest
genCorner ci cj (Ni Air b   Air) rest = (OutCorner ci cj, b) : rest
genCorner ci cj (Ni Air Air c  ) rest = (OutCorner ci cj, c) : rest
genCorner ci cj (Ni _ _ _) rest = rest

--  1
-- 0 2
--  3
genEdge :: Int -> Int -> Int -> Plat -> Pent Material -> Build (ElemCode,Plat)
genEdge si sj side Air _                rest = rest
genEdge si sj side c   (Pent M _ M _ O) rest = (Edge si sj ShortPlusR side, c) : rest
genEdge si sj side c   (Pent O _ M _ M) rest = (Edge si sj ShortPlusL side, c) : rest
genEdge si sj side c   (Pent M _ M _ M) rest = (Edge si sj Short side, c) : rest
genEdge si sj side c   (Pent O _ M _ O) rest = (Edge si sj Long side, c) : rest

{-
genEdge si sj side c   (Pent O M M M O) rest = (Edge si sj Long side, c) : rest
genEdge si sj side c   (Pent _ _ M M O) rest = (Edge si sj ShortPlusR side, c) : rest
genEdge si sj side c   (Pent O M M _ _) rest = (Edge si sj ShortPlusL side, c) : rest
genEdge si sj side c   _                rest = (Edge si sj Short side, c) : rest
-}


-- 0 1 2
-- 7   3
-- 6 5 4
genMicros :: Int2 -> Quad Plat -> Build (MicroCode,[Plat])
genMicros (I2 ci cj) (Quad Air Air Air Air) rest = rest
-- flats
genMicros (I2 ci cj) (Quad a   Air Air Air) rest = (Site ci cj 3 ur, [a]) : (Site ci cj 5 ud, [a]) : rest
genMicros (I2 ci cj) (Quad Air b   Air Air) rest = (Site ci cj 5 ud, [b]) : (Site ci cj 7 ul, [b]) : rest
genMicros (I2 ci cj) (Quad Air Air c   Air) rest = (Site ci cj 1 uu, [c]) : (Site ci cj 3 ur, [c]) : rest
genMicros (I2 ci cj) (Quad Air Air Air d  ) rest = (Site ci cj 1 uu, [d]) : (Site ci cj 7 ul, [d]) : rest
-- flats
genMicros (I2 ci cj) (Quad a   b   Air Air) rest = (Site ci cj 5 ud, [a,b]) : rest
genMicros (I2 ci cj) (Quad a   Air c   Air) rest = (Site ci cj 3 ur, [a,c]) : rest
genMicros (I2 ci cj) (Quad Air b   Air d  ) rest = (Site ci cj 7 ul, [b,d]) : rest
genMicros (I2 ci cj) (Quad Air Air c   d  ) rest = (Site ci cj 1 uu, [c,d]) : rest
-- corners
genMicros (I2 ci cj) (Quad a   Air Air d  ) rest = (Site ci cj 2 uur, [a,d]) : (Site ci cj 6 udl, [a,d]) : rest
genMicros (I2 ci cj) (Quad Air b   c   Air) rest = (Site ci cj 0 uul, [b,c]) : (Site ci cj 4 udr, [b,c]) : rest
-- corners
genMicros (I2 ci cj) (Quad a   b   c   Air) rest = (Site ci cj 4 udr, [b,c]) : rest
genMicros (I2 ci cj) (Quad a   b   Air d  ) rest = (Site ci cj 6 udl, [a,d]) : rest
genMicros (I2 ci cj) (Quad a   Air c   d  ) rest = (Site ci cj 2 uur, [a,d]) : rest
genMicros (I2 ci cj) (Quad Air b   c   d  ) rest = (Site ci cj 0 uul, [b,c]) : rest
genMicros (I2 ci cj) _                      rest = rest



-- LEVEL FILE


readLevelFile :: FilePath -> IO AbstractLevel
readLevelFile path = do
  ls <- fmap lines (readFile path)
  let header:more = ls
  let (w,h) = readLevelHeader header
  let blocks = map readBlockData more
  return (AL w h (gridFromList blocks))

readLevelHeader :: String -> (Int,Int)
readLevelHeader input =
  let stuff = takeStr "level " input in
  let [l,r] = split ' ' stuff in
  (read l, read r)

readBlockData :: String -> ((Int,Int), Char)
readBlockData input =
  let stuff = takeStr "block " input in
  let [c,i,j] = split ' ' stuff in
  ((read i, read j), head c)

split :: Eq a => a -> [a] -> [[a]]
split delim input = case break (== delim) input of
  (chunk,[])       -> [chunk]
  (chunk,(_:more)) -> chunk : split delim more

takeStr :: String -> String -> String
takeStr "" input = input
takeStr (c:cs) (d:ds) = if c==d then takeStr cs ds else error "takeStr failed"
