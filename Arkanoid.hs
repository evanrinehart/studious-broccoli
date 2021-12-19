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

import Debug.Trace

import Control.Exception
import Control.Concurrent

import Common

data Ark = Ark Elements [(Float2,Micro)] Particle
  deriving Show

-- an extended geometric path on the plane, carries label and payload
data Elem = RuleAB Float2 Float2
          | Circle Float2 Float
  deriving Show

-- microsurface view, either 1 normal or 2, refers to elements involved
data Micro = Flat Float2 IntSet
           | Corner Float2 Float2 IntSet
  deriving Show

microElems :: Micro -> IntSet
microElems (Flat _ es) = es
microElems (Corner _ _ es) = es
  
type Elements = IntMap Elem

collisionTest :: Float2 -> Float2 -> Elem -> Maybe (Float,Float2)
collisionTest x v el = fmap g (answerMaybe el) where
  answerMaybe (RuleAB la lb) = particleHitsLineIn x v la lb
  answerMaybe (Circle c r)   = particleHitsCircleIn x v c r
  g dt = let x' = uniform dt x v in (dt,x')


{-
getMicrosite :: MicroMap -> Float2 -> Maybe Micro
getMicrosite mm x = fmap snd $ find (\(y,_) -> norm (x-y) <= 0.001) mm
-}

-- an elem has a flat surface unless micromap indicates unusual feature here
--overlay :: Elem -> MicroMap -> Float2 -> Micro
--overlay el mm x = case getMicrosite mm x of
--  Nothing -> Flat (normalize $ normalToElementAt el x) (IS.singleton (elementNo el))
--  Just u  -> u

-- critical support to get microbeam to work:
--   get a set of possible elements to do a collision test on, using x and v
--   overlay special microsites at x, fallback to given el
--   you can pass in all elements and microsites, or narrow it down first

-- fire the test beam into the geometry.
-- If it hits within the time limit return hit details (time, place, microsurface)

-- a particle is either in free flight or is in the process of colliding
data Particle
  = FParticle Float2 Float2 -- x v
  | CParticle Float2 Float2 Float2 IntSet -- x v0 v1 elements
      deriving Show

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

detect dt (F2 x y) (F2 vx vy) = 
  let x' = x + vx * dt in
  let y' = y + vy * dt in
  if x==x' || y==y' then error (show (dt, F2 x y, F2 vx vy)) else ()

particlePosition :: Particle -> Float2
particlePosition (FParticle x _) = x
particlePosition (CParticle x _ _ _) = x

{-
printParticleFuture :: IntMap Elem -> MicroMap -> Particle -> IO ()
printParticleFuture shapes mm p0 = go p0 where
  go :: Particle -> IO ()
  go p =
    let (p', ces) = particle shapes mm p 1 in do
    let F2 x y = particlePosition p'
    let no = length ces
    print (p', ces)
    --printf "%6.3f %6.3f %d\n" x y no
    threadDelay 1000000
    --print (p', ces)
    go p'
-}







-- level file

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






-- errors. They should only happen in case of a bug.

data ArkanoidException = ArkanoidException String deriving Show
instance Exception ArkanoidException

-- future 

data ArkanoidF a = ArkanoidF
  { arkMouse      :: Float -> a
  , arkLeftClick  :: a
  , arkRightClick :: a }







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



data Quad a = Quad a a a a
data QuadTree a = Leaf Int [a] | QuadSplit Float2 (Quad (QuadTree a))

data Platonic = Paddle Float Float | Block Int Int | Wall Float2 Float2 deriving Show

-- the abstract level is just a layout of walls and blocks
-- the geometry database is derived from the abstract level
-- each element can be followed back to an abstract block or wall
-- (the paddle is a dynamic inclusion in the database)

-- geometry database
-- the geometry database is basically a set of elements with some functionality
-- (microsites are generated from high level data)
-- 1. focus on the first result of a microbeam cast to get a useful db partition
-- 2. combine two databases (level + paddle)
-- 3. remove geometry by abstract block
-- 
-- sets: blocks, elements
-- virtual set: microsite(blocks)
-- relations:
--   element -> platonic
--   block -> [element]
--   microsite -> [element]
-- spatial index: area -> [segment]

-- Operations:
-- remove block
-- list all blocks
-- merge databases (level + paddle)
-- microbeam :: XV -> DB -> Maybe (T, X, MicroSurface, DB1(with only), DB2(without))

removeBlock :: Int -> Int -> Net -> Net
removeBlock i j net = net

allPlatonics :: Net -> [Platonic] -- map all segments to platonic and unique
allPlatonics net = []

merge :: Net -> Net -> Net
merge n1 n2 = n1

netSplitArea :: Float2 -> Float -> Net -> (Net,Net)
netSplitArea x r net = (small, large) where
  ks = IM.keysSet $ IM.filterWithKey (\k el -> nearElement x el) (netElements net)
  small = undefined
  large = undefined
  

netSplit1 :: Int -> Net -> (Net,Net)
netSplit1 k net = (netIsolate k net, netDelete k net)

netDelete :: Int -> Net -> Net
netDelete k (Net els elp ix c) = Net els' elp' ix' c where
  els' = IM.delete k els
  elp' = IM.delete k elp
  ix' = case IM.lookup k elp of
    Just (Block i j) -> gridMod (L.delete k) i j ix
    Nothing          -> ix

netIsolate :: Int -> Net -> Net
netIsolate k (Net els elp ix c) = case IM.lookup k els of
  Nothing -> emptyNet
  Just el -> Net els' elp' ix' c where
    els' = IM.singleton k el
    elp' = IM.singleton k (elp ! k)
    plat = elp ! k
    ix' = case plat of
      Block i j -> gridSingleton i j [k]
      _         -> IM.empty

emptyNet :: Net
emptyNet = Net IM.empty IM.empty IM.empty 0

--data AbstractLevel = AL Int Int (Grid Char) deriving Show
type Grid a = IntMap (IntMap a)

data Net = Net
  { netElements :: IntMap Elem -- low level shapes
  , netElementPlatonic :: IntMap Platonic -- what elements correspond to
  , netBlockElements :: Grid [Int] -- the elements generated by a block
  , netCounter :: Int
  } deriving Show

-- the net tells you if a ray hits anything and where
-- the solids tell you if you hit a microsite with special handling
-- the result is the net split at the hit site

type MicroHit = (Float,(Float2,Micro,(Net,Net)))
type BigHit = (Float,(Float2,Micro,(Net,Net)))

netCollisions :: Float2 -> Float2 -> Net -> [BigHit]
netCollisions baseX v net = catMaybes $ map g (IM.toList (netElements net)) where
  g (i,el) = case collisionTest baseX v el of
    Just (t,x) -> let n = normalToElementAt el x
                  in Just (t, (x, Flat n _, netSplit1 i net))
    Nothing    -> Nothing

microLookup :: Float2 -> Solids -> Maybe (Micro, [Int])
microLookup = error "microlookup"

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

-- "high level" gameplay types
-- AbstractLevel -> Solids
-- AbstractLevel -> Net

data Solids = Solids -- Solids -> XV -> Maybe Micro
  { solidBlocks   :: Grid Char
  , solidPaddleXY :: Float2
  , solidAreaWH   :: Float2
  } deriving Show

data Solids2 = Solids2
  { s2Blocks :: Grid Char
  , s2Pad    :: Maybe Float2
  , s2Top    :: Maybe Float
  , s2Left   :: Maybe Float
  , s2Righ   :: Maybe Float }

makeSolids :: AbstractLevel -> Solids
makeSolids (AL wi hi blocks) = Solids blocks padStart (F2 w h) where
  w = s * fromIntegral wi
  h = s * fromIntegral hi
  bottom = -h / 2
  padStart = F2 0 (bottom + s)
  s = 32

radiusToArea :: Float2 -> Float -> Float4
radiusToArea c r = let F2 x y = c - F2 r r in F4 x y (2*r) (2*r)

overlapping :: Float4 -> Float4 -> Bool
overlapping (F4 x1 y1 w1 h1) (F4 x2 y2 w2 h2) = not apart where
  apart = x2 > x1 + w1 || x1 > x2 + h2 || y2 > y1 + h1 || y1 > y2 + h2

overlappingIntBox :: Int4 -> Int4 -> Bool
overlappingIntBox (I4 x1 y1 w1 h1) (I4 x2 y2 w2 h2) = not apart where
  apart = x2 > x1 + w1 || x1 > x2 + h2 || y2 > y1 + h1 || y1 > y2 + h2

-- this function is intended for points not on a boundary
pointToGridIndex :: Float2 -> Float -> Float2 -> (Int,Int)
pointToGridIndex (F2 aw ah) s (F2 x y) = (0,0)

coveringGrid :: Int4 -> Grid a -> Grid a
coveringGrid area grid = filterGrid g grid where
  g i j _ = overlappingIntBox area (I4 i j 0 0)

  
-- 


netGenInt :: State Net Int
netGenInt = state $ \net@Net{netCounter=n} -> (n, net{netCounter=n+1})

netModElements :: (IntMap Elem -> IntMap Elem) -> State Net ()
netModElements f = modify $ \net@Net{netElements=im} -> net{netElements=f im}

netModElementPlatonic :: (IntMap Platonic -> IntMap Platonic) -> State Net ()
netModElementPlatonic f = modify $ \net@Net{netElementPlatonic=im} -> net{netElementPlatonic=f im}

netModBlockElements :: (Grid [Int] -> Grid [Int]) -> State Net ()
netModBlockElements f = modify $ \net@Net{netBlockElements=im} -> net{netBlockElements=f im}

-- FIXME
dummyEmptyIntMap = IM.empty

makeNet :: AbstractLevel -> Net
makeNet (AL wi hi blocks) = flip execState emptyNet $ do
  let s = 32
  let w = s * fromIntegral wi
  let h = s * fromIntegral hi
  makeAbyss w h
  makeWalls w h
  forM_ (gridAssoc blocks) $ \(i,j,x) -> do
    let els = blockToElements (F2 w h) 5 i j 
    forM_ els $ \el -> do
      n <- netGenInt
      netModElements (IM.insert n el)
      netModElementPlatonic (IM.insert n (Block i j))
      netModBlockElements (gridUpsert i j (n:) [n])

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

makeAbyss w h = do
  n <- netGenInt
  let l = -w/2
  let r = w/2
  let b = -h/2
  let el = RuleAB (F2 r b) (F2 l b)
  netModElements (IM.insert n el)
  netModElementPlatonic (IM.insert n Abyss)

makeWalls w h = do
  let l = -w/2
  let r = w/2
  let bot = -h/2
  let t = h/2
  let a = F2 l bot
  let b = F2 l t
  let c = F2 r t
  let d = F2 r bot
  makeWall a b
  makeWall b c
  makeWall c d

makeWall p q = do
  n <- netGenInt
  let el = RuleAB p q
  netModElements (IM.insert n el)
  netModElementPlatonic (IM.insert n Wall)
  

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

--fromAbstractLevel :: AbstractLevel -> Net
--fromAbstractLevel (AL w h blocks) =

data ConcreteLevel = CL
  { clL :: Float
  , clR :: Float
  , clT :: Float
  , clWH :: Float2
  , clS :: Float -- cell size
  , clBR :: Float -- ball radius
  , clBlocks :: Grid (Float4, Char)
  }

type XV = (Float2,Float2)
type TX = (Float,Float2)
type XR = (Float2,Float)
data Pad = Pad
  { padXY :: Float2
  , padWH :: Float2 }
      deriving Show




-- "one sided lines"
elementIntersect :: Float2 -> Float2 -> Elem -> Maybe (Float,Float2)
elementIntersect x v el =
  if inElementCollisionSpace x el
    then
      let f dt = (dt, uniform dt x v)
      in case el of
        RuleAB la lb -> fmap f (particleHitsLineIn x v la lb)
        Circle c r   -> fmap f (particleHitsCircleIn x v c r)
    else Nothing

inElementCollisionSpace :: Float2 -> Elem -> Bool
inElementCollisionSpace x (RuleAB la lb) = lineSign x la lb < 0
inElementCollisionSpace x (Circle c r) = norm (x - c) > r

-- velocity reflects according to surface normal(s)
-- (or does nothing because v is already headed out)
microInteraction :: Micro -> Float2 -> Float2
microInteraction (Flat n _) v = if n `dot` v < 0 then reflect n v else v
microInteraction (Corner n1 n2 _) v =
  case (v `dot` n1 < 0, v `dot` n2 < 0) of
    (True,   True) -> reflect n1 (reflect n2 v)
    (True,  False) -> reflect n1 v
    (False,  True) -> reflect n2 v
    (False, False) -> v

elementIntersectMany :: Float2 -> Float2 -> [Elem] -> Maybe (Float, Float2, Elem)
elementIntersectMany x v elems =
  let tag el (t,x) = (t,x,el) in
  case catMaybes $ map (\el -> fmap (tag el) (elementIntersect x v el)) elems of
    []   -> Nothing
    hits -> Just $ minimumBy (comparing (\(t,_,_) -> t)) hits
  
