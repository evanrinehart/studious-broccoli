{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLists #-}
module Arkanoid where

import Text.Printf

import Data.Map as M (Map, fromList, toList, lookup)
import Data.IntMap.Strict as IM
  (IntMap, elems, keysSet, fromList, assocs, foldl', filter, minView, minViewWithKey,
  mapMaybeWithKey, mapWithKey, withoutKeys, restrictKeys, singleton, insert, (\\))
import Data.IntSet as IS
  (IntSet, singleton)
import Data.List hiding ((\\))
import Data.Maybe
import Data.Function

import Debug.Trace

import Common

-- an extended geometric path on the plane, carries label and payload
data Elem a = RuleAB Float2 Float2 Int a
            | Circle Float2 Float Int a
  deriving Show

-- microsurface view, either 1 normal or 2, refers to elements involved
data Micro = Flat Float2 IntSet
           | Corner Float2 Float2 IntSet
  deriving Show

microElems :: Micro -> IntSet
microElems (Flat _ es) = es
microElems (Corner _ _ es) = es
  
-- special microsites to augment the natural flats of elements
type MicroMap = [(Float2, Micro)]


data SpaceTimePoint a = STP !Float !Float2 !a
  deriving Show

type Hit = SpaceTimePoint ()
collisionTest :: Float2 -> Float2 -> Elem a -> Maybe Hit
collisionTest x v element = fmap g answerMaybe where
  answerMaybe = case element of
    RuleAB la lb _ _ -> particleHitsLineIn x v la lb
    Circle c r _ _   -> particleHitsCircleIn x v c r
  g dt = let x' = uniform dt x v in STP dt x' ()

willHitElements :: Float2 -> Float2 -> IntMap (Elem a) -> IntMap Hit
willHitElements x v = IM.mapMaybeWithKey (\i sh -> collisionTest x v sh)

findHitTime :: IntMap Hit -> Float
findHitTime = IM.foldl' (\least (STP t _ _) -> min least t) (1/0)

findHitLocation :: Float -> IntMap Hit -> Float2
findHitLocation t hits =
  let STP _ x _ = head (Prelude.filter (\(STP s _ _) -> s==t) (IM.elems hits)) in x

microInteraction :: Micro -> Float2 -> Float2
microInteraction (Flat n _) v = reflect n v
microInteraction (Corner n1 n2 _) v = reflect n1 (reflect n2 v)

microLookup :: MicroMap -> IntMap (Elem a) -> Float2 -> Micro
microLookup mm shapes x = case find (\(y,_) -> norm (x - y) <= 0.001) mm of
  Just (_,u)  -> u
  Nothing -> case IM.minViewWithKey (elemsWithin shapes x 0.001) of
    Just ((i,el),rest) -> case IM.minView rest of
      Nothing -> Flat (normalize $ normalToElementAt el x) (IS.singleton i)
      Just _  -> error "microLookup failed (multiple shapes)"
    Nothing -> error $ "microLookup failed (nothing at " ++ show x ++ ")"

elemsWithin :: IntMap (Elem a) -> Float2 -> Float -> IntMap (Elem a)
elemsWithin shapes searchC searchR = IM.filter f shapes where
  f (RuleAB a b _ _) =
    let x = closestPointOnLine searchC a b in
    let d = norm (searchC - x) in
    if d < searchR
      then let s = lineSegmentParam x a b in 0 <= s && s <= 1
      else False
  f (Circle c r _ _) = let d = norm (c - searchC) in r - searchR < d && d < r + searchR
  

-- a particle is either in free flight or is in the process of colliding
data Particle
  = FParticle Float2 Float2 -- x v
  | CParticle Float2 Float2 Float2 IntSet -- x v0 v1 elements
      deriving Show
type CE = SpaceTimePoint IntSet

particle :: IntMap (Elem a) -> MicroMap -> Particle -> Float -> (Particle, [CE])
particle shapes mm p t1 = go 0 [] p where
  go t accum (FParticle x v)        = go2 t accum x v shapes
  go t accum (CParticle x v0 v1 cc) = go2 t (STP t x cc : accum) x v1 (shapes `IM.withoutKeys` cc)

  go2 :: Float -> [CE] -> Float2 -> Float2 -> IntMap (Elem a) -> (Particle, [CE])
  go2 t accum x v ss =
    let allHits = willHitElements x v ss in -- give a time delta for xv to hit
    let dt = findHitTime allHits in
    let t' = t + dt in
    -- if dt is zero, we're stuck
    let vsLimit = compare t' t1 in
    case vsLimit of
      GT ->
        let x' = uniform (t1 - t) x v in
        (FParticle x' v, accum)
      _ ->
        let hitX = findHitLocation dt allHits in
        let u = microLookup mm shapes hitX in
        let v' = microInteraction u v in
        let es = microElems u in
        let ce = STP t' hitX es in
        case vsLimit of
          LT -> go t' accum (CParticle hitX v v' es)
          EQ -> (CParticle hitX v v' es, accum)

particlePosition :: Particle -> Float2
particlePosition (FParticle x _) = x
particlePosition (CParticle x _ _ _) = x




-- to avoid recognizing microenvironments at runtime, construct the
-- level out of (a way to generate) collision surfaces.
-- i.e. Float2 -> CollisionSurface
-- this can be a two level lookup. If xy is within the location of
-- an unusual collision surface, return that. Otherwise return simple surface.
-- Otherwise, there is no collision surface nearby, and that's a bug.
-- continuity principle: as the point along a shape moves, the collision surface
-- may change discontinuously but the result of hitting there should change smoothly.



-- level: set of unusual locations, set of shapes
-- "compiler" that generates this data from block locations and level walls


-- assuming the set of shapes is not empty
-- and xy is generally on all the surfaces
-- and the pattern of coincident shapes is supported
--simplifyMicrosurface :: IntMap Shape -> Float2 -> CollisionSurface
--simplifyMicrosurface shapes xy = 



--collisionSurfaceAt :: IntMap Shape -> Float2 -> Float -> Maybe CollisionSurface
--collisionSurfaceAt shapes c r = 



{-
particleFuture :: IntMap Shape -> Particle -> [CE]
particleFuture shapes p0 = concat (go p0) where
  go p = let (p', ces) = particle shapes p 1 in ces : go p'
-}

printParticleFuture :: IntMap (Elem a) -> MicroMap -> Particle -> IO ()
printParticleFuture shapes mm p0 = go p0 where
  go :: Particle -> IO ()
  go p =
    let (p', ces) = particle shapes mm p 0.5 in do
    let F2 x y = particlePosition p'
    let no = length ces
    printf "%4f %4f %d\n" x y no
    --print (p', ces)
    go p'

{-
-- 
testManyShapes :: Float2 -> Float2 -> ShapeSet -> Maybe (Float, Float2, ShapeSet)
testManyShapes x v ss = collectWinnings $ catMaybes $ map h (IM.toList ss) where
  ctw x v i sh = fmap g (collisionTest x v sh) where
    g (t,x') = (t, i, sh, normalToElementAt sh x')
  h (i,sh) = ctw x v i sh

-- take the winning collisions, if any
-- return their time, net normal, and shape set
collectWinnings :: [(Float,Int,Shape,Float2)] -> Maybe (Float, Float2, ShapeSet)
collectWinnings [] = Nothing
collectWinnings ((t,i0,sh0,n0):more) = go n0 (IM.singleton i0 sh0) more where
  go !accum !set ((u,i,sh,n):xs)
    | u /= t    = Just (t, normalize accum, set)
    | otherwise = go (n + accum) (IM.insert i sh set) xs
  go !accum !set [] = Just (t, normalize accum, set)

-}





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

--hitsInTime :: Float -> (Float,Float2) -> Maybe (Float,Float2)
--hitsInTime limit (dt,x) = if limit 

normalize x = x ./ norm x

-- not normalized. circles normal points out. S to N line normal points east.
normalToElementAt (RuleAB la lb _ _) _ = lineNormal la lb
normalToElementAt (Circle c r _ _) x = x - c

lineNormal la lb = let F2 x y = lb - la in F2 y (-x)

closestPointOnLine x la lb = 
  let d = la - lb in
  let n = d ./ norm d in
  let s = (x - la) `dot` n in
  la + s *. n

distanceToLine x la lb = let y = closestPointOnLine x la lb in norm (x - y)

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

uniform :: Float -> Float2 -> Float2 -> Float2
uniform dt x v = x + v .* dt

norm (F2 x y) = sqrt (x*x + y*y)
dot (F2 a b) (F2 c d) = a*c + b*d

cross2 (F2 a b) (F2 c d) = a*d - b*c

(F2 x y) ./ s = F2 (x/s) (y/s)
s *. (F2 x y) = F2 (s*x) (s*y)
(F2 x y) .* s = F2 (x*s) (y*s)

infixl 7 .*
infixl 7 *.




-- test data

box :: a -> IntMap (Elem a)
box x = IM.fromList [(0, l), (1, t), (2, r), (3, bb)] where
  l = RuleAB a b 0 x
  t = RuleAB b c 1 x
  r = RuleAB c d 2 x
  bb = RuleAB d a 3 x
  a = F2 0 0
  b = F2 0 10
  c = F2 10 10
  d = F2 10 0

corners :: [(Float2, Micro)]
corners = [(a,c1),(b,c2),(c,c3),(d,c4)] where
  c1 = Corner nu nr [0,3]
  c2 = Corner nr nd [0,1]
  c3 = Corner nd nl [1,2]
  c4 = Corner nu nl [2,3]
  a = F2 0 0
  b = F2 0 10
  c = F2 10 10
  d = F2 10 0
  nu = normalize $ lineNormal d a
  nr = normalize $ lineNormal a b
  nd = normalize $ lineNormal b c
  nl = normalize $ lineNormal c d


-- future 

data ArkanoidF a = ArkanoidF
  { arkMouse      :: Float -> a
  , arkLeftClick  :: a
  , arkRightClick :: a }

data Block = Block
  { blockType :: Int
  , blockHP   :: Int }

data Ball = Ball
  { ballXV   :: (Float2, Float2) }

data S = S
  { sMouse    :: [Float] -- last N mouse locations
  , sBalls    :: [Ball]
  , sPowerups :: [(Float2,Float2)]
  --, sBlocks   :: SpaceIx Block   -- get blocks in a region
  }



-- experimental

{-
data TimeVsLimit = WayBefore | WayAfter | AtLimit deriving Show
-- compare a time point to a time limit
-- LT = before, resolve a collision
-- GT = way after, don't do anything
-- EQ = collision happens near the time limit, collision in progress
compareToTimeLimit :: Float -> Float -> TimeVsLimit
compareToTimeLimit t limit
  | t < limit         = WayBefore -- "before = way before"
  | t == limit        = AtLimit
  | otherwise         = WayAfter -- "way after"
-}
