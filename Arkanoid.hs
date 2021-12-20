{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLists #-}
module Arkanoid where

import Text.Printf

import Data.Map as M (Map, fromList, toList, lookup)
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict ((!))
import Data.IntMap.Strict as IM
  (IntMap, elems, keysSet, fromList, assocs, filter, minView, minViewWithKey, empty, unions,
  mapMaybeWithKey, mapWithKey, mapMaybe, withoutKeys, restrictKeys, singleton, insert, (\\))
import Data.IntSet as IS
  (IntSet, singleton)
import Data.List hiding ((\\))
import Data.Ord
import qualified Data.List as L
import Data.Maybe
import Data.Function
import Data.Foldable
import Control.Monad.State
import Control.Monad.Writer

import Debug.Trace

import Control.Exception
import Control.Concurrent

import Common

-- an extended geometric path on the plane, carries label and payload
data Elem = RuleAB Float2 Float2 | Circle Float2 Float
  deriving Show

-- microsurface view, either 1 normal or 2, refers to elements involved
data Micro = Flat Float2 | Corner Float2 Float2
  deriving Show

data Particle = P !Float2 !Float2
  deriving Show

data ArkanoidException = ArkanoidException String
  deriving Show

instance Exception ArkanoidException


particlePosition :: Particle -> Float2
particlePosition (P x _) = x

-- GEOMETRY STUFF
{-
closestPointOnCircleToLine :: Circle -> Float2 -> Float2 -> Float2
closestPointOnCircleToLine (Circle c r) la lb =
  let p = closestPointOnLine c la lb in
  let d = p - c in
  c + r *. (d ./ norm d)
-}

-- line cross x negative, you're "on the right"
-- x cross line negative, you're "on the left"
lineSign :: Float2 -> Float2 -> Float2 -> Float
lineSign x la lb =
  let d = lb - la in
  let p = x - la in
  signum (p `cross2` d)

-- how fast is x moving toward line
-- abruptly switches sign when passing the line
altitudeRate x v la lb =
  let d = lb - la in
  let n = d ./ norm d in
  let p = x - la in
  let sign = signum (p `cross2` d) in
  sign * (v `cross2` n)

timeUntilCircle :: Float2 -> Float2 -> Float2 -> Float -> Maybe (Float,Float)
timeUntilCircle x v c r = 
  let a = (v `dot` v) in
  let b  = 2 * (v `dot` (x - c)) in
  let cc = (c `dot` c) + (x `dot` x) - 2*(c `dot` x) - r*r in
  let h = b*b - 4*a*cc in
  if h < 0
    then Nothing
    else
      let ans1 = (-b + sqrt h) / (2*a) in
      let ans2 = (-b - sqrt h) / (2*a) in
      Just (min ans1 ans2, max ans1 ans2)

circleDebug :: Float2 -> Float2 -> Float2 -> Float -> IO ()
circleDebug x v c r = do
  let a = (v `dot` v)
  let b  = 2 * (v `dot` (x - c))
  let cc = (c `dot` c) + (x `dot` x) - 2*(c `dot` x) - r*r
  let h = b*b - 4*a*cc
  printf "a = %s\n" (show a)
  printf "b = %s\n" (show b)
  printf "cc = %s\n" (show cc)
  printf "h = %s\n" (show h)
  printf "b**2 = %s\n" (show (b*b))
  printf "4ac = %s\n" (show (4*a*cc))
  
  

timeUntilIntersection :: Float2 -> Float2 -> Float2 -> Float2 -> Maybe Float
timeUntilIntersection x v la lb =
  let v' = altitudeRate x v la lb in
  if v' < 0
    then Just $ norm (closestPointOnLine x la lb - x) / (-v')
    else Nothing

-- this assumes the line segment is stationary
particleHitsLineIn :: Float2 -> Float2 -> Float2 -> Float2 -> Maybe Float
particleHitsLineIn x v la lb =
  let d = closestPointOnLine x la lb - x in
  let v' = altitudeRate x v la lb in
  if d==0 || v' < 0 -- on line or moving toward line
    then
      let dt = if d==0 then 0 else norm d / (-v') in
      let p = x + v .* dt  in
      let s = lineSegmentParam p la lb in
      if s >= 0 && s <= 1
        then Just dt
        else Nothing -- passes outside extent of segment
    else
      Nothing

particleHitsCircleIn :: Float2 -> Float2 -> Float2 -> Float -> Maybe Float
particleHitsCircleIn x v c r = case timeUntilCircle x v c r of
  Nothing -> Nothing
  Just (t1,t2)
    | t2 < 0 -> Nothing
    | t1 < 0 -> Just t2
    | otherwise -> Just t1

normalize x = x ./ norm x

-- not normalized. circles normal points out. S to N line normal points east.
normalToElementAt (RuleAB la lb) _ = lineNormal la lb
normalToElementAt (Circle c r) x = x - c

lineNormal la lb = let F2 x y = lb - la in F2 y (-x)

closestPointOnLine x la lb = 
  let d = la - lb in
  let n = d ./ norm d in
  let s = (x - la) `dot` n in
  la + s *. n

distanceToLine x la lb = let y = closestPointOnLine x la lb in norm (x - y)


nearElement :: Float2 -> Elem -> Bool
nearElement x (Circle c r) = let d = norm (c - x) in r - 0.001 < d && d < r + 0.001
nearElement x (RuleAB a b) =
  distanceToLine x a b < 0.001 &&
    let s = lineSegmentParam x a b in 0 <= s && s <= 1


-- x between la lb returns 0=la, 1=lb
lineSegmentParam x la lb =
  let xla = x - la in
  let lbla = lb - la in
  let bigL = norm lbla in
  let n = lbla ./ bigL in
  (xla `dot` n) / bigL

-- n must be normalized
reflect :: Float2 -> Float2 -> Float2
reflect n v = v - (2 * v `dot` n) *. n

norm (F2 x y) = sqrt (x*x + y*y)
dot (F2 a b) (F2 c d) = a*c + b*d

cross2 (F2 a b) (F2 c d) = a*d - b*c

(F2 x y) ./ s = F2 (x/s) (y/s)
s *. (F2 x y) = F2 (s*x) (s*y)
(F2 x y) .* s = F2 (x*s) (y*s)

infixl 7 .*
infixl 7 *.

propogate dt (P x v) = P (x + v .* dt) v










-- GRID

type Grid a = IntMap (IntMap a)

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


radiusToArea :: Float2 -> Float -> Float4
radiusToArea c r = let F2 x y = c - F2 r r in F4 x y (2*r) (2*r)

overlapping :: Float4 -> Float4 -> Bool
overlapping (F4 x1 y1 w1 h1) (F4 x2 y2 w2 h2) = not apart where
  apart = x2 > x1 + w1 || x1 > x2 + h2 || y2 > y1 + h1 || y1 > y2 + h2

overlappingIntBox :: Int4 -> Int4 -> Bool
overlappingIntBox (I4 x1 y1 w1 h1) (I4 x2 y2 w2 h2) = not apart where
  apart = x2 > x1 + w1 || x1 > x2 + h2 || y2 > y1 + h1 || y1 > y2 + h2


data ConcreteLevel a = CL
  { clL :: Float
  , clR :: Float
  , clT :: Float
  , clWH :: Float2
  , clS :: Float -- cell size
  , clBR :: Float -- ball radius
  , clBlocks :: Grid (Float4, Char)
  }


-- PARTICLE PHYSICS

-- "one sided lines"
elementIntersect :: Float2 -> Float2 -> Elem -> Maybe (Float,Float2)
elementIntersect x v el =
  if inElementCollisionSpace x el
    then
      let f dt = (dt, x + v .* dt) in
      case el of
        RuleAB la lb -> fmap f (particleHitsLineIn x v la lb)
        Circle c r   -> fmap f (particleHitsCircleIn x v c r)
    else Nothing

inElementCollisionSpace :: Float2 -> Elem -> Bool
inElementCollisionSpace x (RuleAB la lb) = lineSign x la lb > 0
inElementCollisionSpace x (Circle c r) = norm (x - c) > r

-- velocity reflects according to surface normal(s)
-- (or does nothing because v is already headed out)
microInteraction :: Micro -> Float2 -> Float2
microInteraction (Flat n) v = if n `dot` v < 0 then reflect n v else v
microInteraction (Corner n1 n2) v =
  case (v `dot` n1 < 0, v `dot` n2 < 0) of
    (True,   True) -> reflect n1 (reflect n2 v)
    (True,  False) -> reflect n1 v
    (False,  True) -> reflect n2 v
    (False, False) -> v

firstElementIntersection :: Float2 -> Float2 -> [(Elem,a)] -> Maybe (Float, Float2, Elem, a)
firstElementIntersection x v elements =
  let tag (el,z) (t,x) = (t,x,el,z) in
  let test p@(el,_) = fmap (tag p) (elementIntersect x v el) in
  case catMaybes (map test elements) of
    []   -> Nothing
    hits -> Just (minimumBy (comparing (\(t,_,_,_) -> t)) hits)

firstElementIntersectionLimited :: Float2 -> Float2 -> Float -> [(Elem,a)] -> Maybe (Float, Float2, Elem, a)
firstElementIntersectionLimited x v limit elements = case firstElementIntersection x v elements of
  Nothing -> Nothing
  Just (t,x,el,z) -> if t <= limit then Just (t,x,el,z) else Nothing

data At a = At
  { atT :: !Float
  , atX :: !Float2
  , atStuff :: [a] }
      deriving Show

data Env a = Env
  { envMacroScan :: Float2 -> Float -> [(Elem,a)]
  , envMicroScan :: Float2 -> Maybe (Micro, [a]) }

particle :: Show a => Env a -> Float -> Particle -> Writer [At a] Particle
particle env limit p_initial = loop 0 p_initial where
  loop t p@(P x v) = do
    let radius = norm v * (limit - t)
    let elements = envMacroScan env x radius
    case firstElementIntersectionLimited x v limit elements of
      Nothing -> return (propogate (limit - t) p)
      Just (dt, hitX, el, a) -> do
        let t'     = t + dt
        let (u,as) = resolveMicroStuff env el a hitX
        let v'     = microInteraction u v
        tell [At t' hitX as]
        if t' < limit
          then loop t' (P hitX v')
          else return (P hitX v')

resolveMicroStuff :: Env a -> Elem -> a -> Float2 -> (Micro, [a])
resolveMicroStuff env el z x = case envMicroScan env x of
  Nothing      -> (Flat (normalize $ normalToElementAt el x), [z])
  Just (u, zs) -> (u, zs)


data Solid = Wall | Pad | Block Int Int deriving Show
makeEnv :: Float -> AbstractLevel -> Env Solid
makeEnv padx (AL w h blocks) = Env elscan uscan where
  a = F2 (-160) (-240)
  b = F2 (-160) 240
  c = F2 160 240
  d = F2 160 (-240)
  elscan x r = [(RuleAB a b, Wall),(RuleAB b c, Wall)
               ,(RuleAB c d, Wall),(RuleAB d a, Wall)] :: [(Elem,Solid)]
  uscan x = if norm (x - F2 160 240) < 0.001
    then Just (corn, [Wall,Wall] :: [Solid]) 
    else Nothing
      where corn = Corner (F2 (-1) 0) (F2 0 (-1))


{-
microBeam :: Net -> Solids -> Float2 -> Float2 -> Float -> Maybe MicroHit
microBeam net solids x v limit = fmap analyze hit where
  hits = netCollisions x v net
  hit = beforeLimit (soonestOf (noHit : hits))

  analyze z@(t,(x,_,_)) = case microLookup x solids net of
    Just (u, is) -> (t,(x,u,netSplit is net))
    Nothing      -> z

  soonestOf = minimumBy (comparing fst)
  beforeLimit p@(t,_) = if t < limit then Just p else Nothing
  noHit = (1/0, bug)
  bug = error "(bug) microBeam placeholder"
-}

{-
particle :: Net -> Solids -> Particle -> Float -> (Particle, [(Float,(Float,Platonic))])
particle allElements mm p t1 = go 0 [] p where
  go t accum (FParticle x v)        = go2 t accum x v allElements
  go t accum (CParticle x v0 v1 plats) = go2 t ((t,(x,plats)) : accum) x v1 (allElements `IM.withoutKeys` cs)

  go2 t accum x v targets = case microBeam targets mm x v t1 of
    Nothing -> 
      -- issue, if t1 - t is too small, x' = x
      -- the supposed free particle may be left at the collision site
      -- i.e. final collision was too recent
      let issue = detect (t1 - t) x v in
      let x' = issue `seq` uniform (t1 - t) x v in
      (FParticle x' v, accum)
    Just (dt,(hitX,u,_)) ->
      -- (non?)issue, if dt is too small, t' = t
      -- though... we don't do dynamics with t here
      -- particle uses hitX and u to determine where to go next
      let t' = t + dt in
      let v' = microInteraction u v in
      let es = microElems u in
      let ce = STP t' hitX es in
      case compare t' (t1 - 0.001) of
        LT -> go t' accum (CParticle hitX v v' es)
        _  -> (CParticle hitX v v' es, accum)
-}


-- ???
blockToElements :: Float2 -> Float -> Int -> Int -> [Elem]
blockToElements (F2 w h) r i j =
  let top  = h/2
      left = -w/2
      s = 32
      f2 m n = F2 (fi m) (h - fi n)
      a = f2 (s * (i+0)) (s * (j+1))
      b = f2 (s * (i+0)) (s * (j+0))
      c = f2 (s * (i+1)) (s * (j+0))
      d = f2 (s * (i+1)) (s * (j+1))
      padl p q = let z = F2 r 0 in RuleAB (p - z) (q - z)
      padr p q = let z = F2 r 0 in RuleAB (p + z) (q + z)
      padu p q = let z = F2 0 r in RuleAB (p - z) (q - z)
      padd p q = let z = F2 0 r in RuleAB (p + z) (q + z)
      circle x = Circle x r
   in [padd a d, padr d c, padu c b, padl b a
      ,circle a, circle b, circle c, circle d]


-- LEVEL FILE

data AbstractLevel = AL Int Int (Grid Char) deriving Show

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
split s = go where
  go xs = case break (==s) xs of
    (chunk,[])   -> [chunk]
    (chunk,more) -> chunk : go (dropOnly s more)
  dropOnly s y@(x:xs) = if x == s then xs else y

takeStr :: String -> String -> String
takeStr "" input = input
takeStr (c:cs) (d:ds) = if c==d then takeStr cs ds else error "takeStr failed"
