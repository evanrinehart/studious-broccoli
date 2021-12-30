{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
--{-# LANGUAGE OverloadedLists #-}
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

import Test.QuickCheck

import Common
import Grid
import Geometry

import Level
import Event


data Ark = Ark
  { arkLevel :: AbstractLevel
  , arkBall  :: !Particle
  , arkPadX  :: !Float }
  --, arkPadXS :: [Float] }
      deriving Show

data ArkParams = AP
  { apWorld :: WorldDimensions
  , apPadWH :: Float2
  , apPadY  :: Float
  , apBallRadius :: Float
  , apBallReset :: (Float2, Float2) }

--newArk lvl = Ark lvl (P (F2 140.9538931 ((-15.65797986)+2.95048)) (F2 0.939692621 (-0.342020143))) 0
newArk lvl = Ark lvl (P (F2 0 0) (F2 5 6)) 0

-- mouse motion
arkMouse :: ArkParams -> Float -> Ark -> (Ark, [At [Plat]])
arkMouse ap mx ark = result where -- TODO
  padx = arkPadX ark
  WD _ (F2 ww wh) _ = apWorld ap
  F2 padw _ = apPadWH ap
  padx' = min (ww/2 - padw/2) (max ((-ww/2) + padw/2) mx) -- clamp could be based on abstract level
  result = resolveOverlap ap ark{ arkPadX = padx' }

-- time
arkTime :: ArkParams -> Float -> Ark -> (Ark, [At [Plat]])
arkTime ap limit ark =
  let ball = arkBall ark
      lvl = arkLevel ark
      padx = arkPadX ark
      --hist = arkPadXS ark
      env = makeEnv ap padx lvl
      (ball', hits) = runWriter (particle env 1 ball)
      P (F2 padx' _) _ = ball'
  in (ark{ arkBall = ball', arkPadX = padx}, hits)


arkActions :: ArkParams -> UserActions (Ark -> (Ark, [At [Plat]]))
arkActions ap = (pure (\ark -> (ark,[]))) { uaMouse = \x _ -> arkMouse ap x }

estimateVelocity :: [Float] -> Float
estimateVelocity [a,b,c] = (a - c) / 2


-- if the bat is overlapping the ball
-- then move the ball and decide what to do with velocity
-- avoid putting the ball through a wall, please
resolveOverlap :: ArkParams -> Ark -> (Ark, [At [Plat]])
resolveOverlap ap ark@(Ark al (P bx bv) padx) =
  let hull = padToHull ap padx
  in case depthWithinHull bx hull of
    Nothing -> (ark, [])
    Just d  ->
      let bv' = if bv `dot` d < 0 then reflect (normalize d) bv else bv -- this behavior sucks
          bx' = bx + d .* 1.05
      in (Ark al (P bx' bv') padx, [At 0 bx [Pad]])

outOfBounds :: ArkParams -> Float2 -> Bool
outOfBounds ap (F2 x y) =
  let WD _ (F2 w h) _ = apWorld ap
      r = apBallRadius ap
  in x - r <= -w/2 || x + r >= w/2 || y + r >= h/2


-- PARTICLE PHYSICS

data Particle = P !Float2 !Float2
  deriving Show

particlePosition :: Particle -> Float2
particlePosition (P x _) = x

propagateX dt x v    = x + v .* dt
propagate dt (P x v) = P (x + v .* dt) v

-- how fast is x moving toward line
-- abruptly switches sign when passing the line
altitudeRate :: Float2 -> Float2 -> Float2 -> Float2 -> Float
altitudeRate x v la lb =
  let d = lb - la in
  let n = d ./ norm d in
  let p = x - la in
  let sign = signum (p `cross2` d) in
  sign * (v `cross2` n)

particleHitsMovingInfiniteLine :: Float2 -> Float2 -> Float2 -> Float2 -> Float2 -> Maybe (Float,Float2)
particleHitsMovingInfiniteLine x v la lb lv = let v' = v - lv in
  if v' `cross2` (lb - la) > 0
    then Nothing
    else
      let vspeed = altitudeRate x v' la lb
          p = closestPointOnLine x la lb 
          h = norm (x - p)
      in if h == 0
        then Just (0,x)
        else if vspeed < 0
          then let dt = negate (h / vspeed) in Just (dt, x + v' .* dt + lv .* dt)
          else Nothing

-- if you're on the line, dt=zero
-- if your coming from the wrong side, never hits
particleHitsInfiniteLine :: Float2 -> Float2 -> Float2 -> Float2 -> Maybe (Float,Float2)
particleHitsInfiniteLine x v la lb = if v `cross2` (lb - la) > 0 then Nothing else
  let vspeed = altitudeRate x v la lb
      p = closestPointOnLine x la lb 
      h = norm (x - p)
  in if h == 0
    then Just (0,x)
    else if vspeed < 0
      then let dt = negate (h / vspeed) in Just (dt, x + v .* dt)
      else Nothing

particleHitsLine :: Float2 -> Float2 -> Float2 -> Float2 -> Maybe (Float,Float2)
particleHitsLine x v la lb = case particleHitsInfiniteLine x v la lb of
  Just (t, p) ->
    let s = lineSegmentParam p la lb in
    if 0 <= s && s <= 1
      then Just (t, p)
      else Nothing
  Nothing -> Nothing

particleHitsMovingLine :: Float2 -> Float2 -> Float2 -> Float2 -> Float2 -> Maybe (Float,Float2)
particleHitsMovingLine x v la lb lv = case particleHitsMovingInfiniteLine x v la lb lv of
  Just (t, p) ->
    let s = lineSegmentParam p (la + lv .* t) (lb + lv .* t) in
    if 0 <= s && s <= 1
      then Just (t, p)
      else Nothing
  Nothing -> Nothing

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

-- if you're travling away from the center, never hits
particleHitsCircle :: Float2 -> Float2 -> Float2 -> Float -> Maybe (Float,Float2)
particleHitsCircle x v c r = if v `dot` (x - c) > 0 then Nothing else
  case timeUntilCircle x v c r of
    Nothing -> Nothing
    Just (t1,t2)
      | t2 < 0    -> Nothing
      | t1 < 0    -> Just (t2, propagateX t2 x v)
      | otherwise -> Just (t1, propagateX t1 x v)

particleHitsMovingCircle :: Float2 -> Float2 -> Float2 -> Float -> Float2 -> Maybe (Float,Float2)
particleHitsMovingCircle x v c r cv = let v' = v - cv in
  if v' `dot` (x - c) > 0
    then Nothing
    else
      case timeUntilCircle x v' c r of
        Nothing -> Nothing
        Just (t1,t2)
          | t2 < 0    -> Nothing
          | t1 < 0    -> Just (t2, propagateX t2 x v' + cv .* t2)
          | otherwise -> Just (t1, propagateX t1 x v' + cv .* t1)


-- "one sided lines"
intersectElement :: Float2 -> Float2 -> Elem -> Maybe (Float,Float2)
intersectElement x v el =
  if not (inElementCollisionSpace x el)
    then Nothing
    else case el of
      RuleAB la lb -> particleHitsLine x v la lb
      Circle c r   -> particleHitsCircle x v c r

intersectMovingElement :: Float2 -> Float2 -> Elem -> Float2 -> Maybe (Float,Float2)
intersectMovingElement x v el ev =
  if not (inElementCollisionSpace x el)
    then Nothing
    else case el of
      RuleAB la lb -> particleHitsMovingLine x v la lb ev
      Circle c r   -> particleHitsMovingCircle x v c r ev

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

movingReflect :: Float2 -> Float2 -> Float2 -> Float2
movingReflect n uv v = reflect n (v - uv) + uv

movingMicroInteraction :: Micro -> Float2 -> Float2 -> Float2
movingMicroInteraction (Flat n) v uv =
  if n `dot` v < 0
    then movingReflect n uv v -- must change and use uv
    else v
movingMicroInteraction (Corner n1 n2) v uv =
  case (v `dot` n1 < 0, v `dot` n2 < 0) of
    (True,   True) -> (movingReflect n1 uv . movingReflect n2 uv) v
    (True,  False) -> movingReflect n1 uv v
    (False,  True) -> movingReflect n2 uv v
    (False, False) -> v

data Battle a = Battle Float Float2 Micro Int a 
  deriving Show

instance Ord (Battle a) where
  compare (Battle t1 _ _ lvl1 _) (Battle t2 _ _ lvl2 _) = compare t1 t2 <> compare lvl1 lvl2

instance Eq (Battle a) where
  b1 == b2 = case compare b1 b2 of
    EQ -> True
    _  -> False

battle :: Show a => Float -> [At (Elem,a)] -> [At (Micro,[a])] -> Maybe (At (Micro,[a]))
battle limit ehits uhits = 
  let mergingA = map f ehits
      mergingB = map g uhits
      f (At t x (el,z)) = Battle t x (Flat (normalToElementAt el x)) 1 [z]
      g (At t x (u,zs)) = Battle t x u 0 zs
      combatants = mergingA ++ mergingB
      Battle t x u _ zs = if null combatants
        then let o = undefined in Battle (1/0) o o o o
        else minimum (traceShow combatants combatants)
  in if t <= limit
    then Just (At t x (u, zs))
    else Nothing

movingBattle :: Show a => Float -> [At ((Elem,Float2),a)] -> [At ((Micro,Float2),[a])] -> Maybe (At (Micro, Float2, [a]))
movingBattle limit ehits uhits = 
  let mergingA = map f ehits
      mergingB = map g uhits
      f (At t x ((el,ev),z)) = Battle t x (Flat (normalToElementAt el x)) 1 (ev,[z])
      g (At t x ((u,uv),zs)) = Battle t x u 0 (uv,zs)
      combatants = mergingA ++ mergingB
      Battle t x u _ ~(surfaceV,zs) = if null combatants
        then let o = undefined in Battle (1/0) o o o o
        else minimum combatants
  in if t <= limit
    then Just (At t x (u, surfaceV, zs))
    else Nothing

intersectElements :: Particle -> [(Elem,a)] -> [At (Elem,a)]
intersectElements (P x v) elements = 
  let g el z (t,p) = At t p (el, z)
      f (el,z) = fmap (g el z) (intersectElement x v el)
  in catMaybes (map f elements)

intersectMovingElements :: Particle -> [((Elem,Float2),a)] -> [At ((Elem,Float2),a)]
intersectMovingElements (P x v) elements = 
  let g el ev z (t,p) = At t p ((el,ev), z)
      f ((el,ev),z) = fmap (g el ev z) (intersectMovingElement x v el ev)
  in catMaybes (map f elements)


data At a = At
  { atT :: !Float
  , atX :: !Float2
  , atStuff :: a }
      deriving (Functor,Show)

data Env a = Env
  { envMacroScan :: Float2 -> Float  -> [((Elem,Float2),a)]
  , envMicroScan :: Float2 -> Float2 -> [At ((Micro,Float2),[a])] } -- see notes on microscanning

particle :: Show a => Env a -> Float -> Particle -> Writer [At [a]] Particle
particle env limit p_initial = loop 0 p_initial where
  loop t p@(P x v) = do
    let radius = norm v * (limit - t) * 1.05
    let elements = envMacroScan env x radius
    let hits = intersectMovingElements p elements
    let microhits = envMicroScan env x v
    case movingBattle limit hits microhits of -- pick first event that's not after the limit
      Nothing -> return (propagate (limit - t) p)
      Just (At dt hitX (u, surfaceV, zs)) -> do
        let t' = t + dt
        let v' = movingMicroInteraction u v surfaceV
        let p' = P hitX v'
        tell [At t' hitX zs]
        if t' < limit
          then loop t' p'
          else return p'

{-
particle :: Show a => Env a -> Float -> Particle -> Writer [At [a]] Particle
particle env limit p_initial = loop 0 p_initial where
  loop t p@(P x v) = do
    let radius = norm v * (limit - t) * 1.05
    let elements = map (\((el,_),a) -> (el,a)) (envMacroScan env x radius)
    let hits = intersectElements p elements
    let microhits = map (fmap (\((u,_),as) -> (u,as))) (envMicroScan env x v)
    case battle limit hits microhits of -- pick first event that's not after the limit
      Nothing -> return (propagate (limit - t) p)
      Just (At dt hitX (u, zs)) -> do
        let t' = t + dt
        let v' = microInteraction u v
        let p' = P hitX v'
        tell [At t' hitX zs]
        if t' < limit
          then loop t' p'
          else return p'
-}

{-
debugParticle :: Show a => Env a -> Float -> Particle -> [DebugParticle]
debugParticle env limit p_initial = loop 0 p_initial where
  loop t p@(P x v) =
    let radius = norm v * (limit - t) * 1.05
--        elements = let foo = envMacroScan env x radius in traceShow foo foo
        elements = envMacroScan env x radius
--        hits = let foo = intersectElements p elements in traceShow foo foo
        hits = intersectElements p elements
--        microhits = let foo = envMicroScan env x v in traceShow foo foo
        microhits = envMicroScan env x v
    in case battle limit hits microhits of
      Nothing -> let p' = propagate (limit - t) p in [DP p (limit-t) [] Nothing p']
      Just (At dt hitX (u, zs)) ->
        let t' = t + dt
            v' = microInteraction u v
            p' = P hitX v'
        in if t' < limit
          then DP p dt [] (Just (dt,hitX,u)) p' : loop t' p'
          else [DP p dt [] (Just (dt,hitX,u)) p']
-}

data DebugParticle = DP Particle Float [Elem] (Maybe (Float,Float2,Micro)) Particle
  deriving Show

packedParticle :: Particle -> String
packedParticle (P x v) = concat ["<P ",packedF2 x," ",packedF2 v,">"] where
  packedF2 (F2 a b) = concat [show a, " ", show b]

dpprint :: DebugParticle -> IO ()
dpprint (DP p rad els result p') = do
  putStrLn $ case result of
    Nothing -> "p=" ++ packedParticle p ++ " rad=" ++ show rad ++ " els=" ++ show (length els) ++ " p'=" ++ packedParticle p'
    Just (dt,hitX,el) -> "p=" ++ packedParticle p ++ " rad=" ++ show rad ++ 
                          " els=" ++ show (length els) ++ " p'=" ++ packedParticle p' ++ " hit=" ++ show (dt,hitX,el)



leavingMicroSite :: Float2 -> Micro -> Bool
leavingMicroSite v (Flat n) = v `dot` n >= 0
leavingMicroSite v (Corner n1 n2) =
  (v `dot` n1 >= 0) &&
  (v `dot` n2 >= 0) 


data PadData = PD
  { pdXY :: !Float2
  , pdWH :: !Float2
  , pdLateralV :: !Float }
      deriving Show

makePadData :: Float -> PadData
makePadData x = PD (F2 x (-150)) (F2 50 10) 0

padDataToBox :: ArkParams -> Float -> Float4
padDataToBox ap padx = F4 (x - w/2) (y - h/2) w h where
  x = padx
  y = apPadY ap
  F2 w h = apPadWH ap

concretizePad :: ArkParams -> Float -> [Elem]
concretizePad ap padx =
  let F4 x y w h = padDataToBox ap padx
      r = apBallRadius ap
      a = F2 x y
      b = F2 x (y + h)
      c = F2 (x + w) (y + h)
      d = F2 (x + w) y
      ud = F2 0 r
      lr = F2 r 0
  in [RuleAB (c+ud) (b+ud), RuleAB (b-lr) (a-lr), RuleAB (a-ud) (d-ud), RuleAB (d+lr) (c+lr)
     ,Circle a r, Circle b r, Circle c r, Circle d r]

padToHull :: ArkParams -> Float -> [RC]
padToHull ap padx =
  let F4 x y w h = padDataToBox ap padx
      r = apBallRadius ap
      a = F2 x y
      b = F2 x (y + h)
      c = F2 (x + w) (y + h)
      d = F2 (x + w) y
      nu = F2 0 1
      nd = -(F2 0 1)
      nl = -(F2 1 0)
      nr = F2 1 0
  in [RC r b nu nl, RC r c nu nr, RC r d nd nr, RC r a nd nl]

makeEnv :: ArkParams -> Float -> AbstractLevel -> Env Plat
makeEnv ap padx al = Env elscan uscan where
  wd = apWorld ap
  elscan x r = padElems ++ (map f . M.toList . genElemCodes al) ixs where
    ixs = radiusToIndices wd x r
    f (code, plat) = ((concretizeElemCode wd code,0), plat)
    padElems = map (\e -> ((e,0), Pad)) (concretizePad ap padx)
  uscan x v  = important where
    haystack = (map g . M.toList . genMicroCodes al) (rayToCorners wd x v)
    important = concatMap f haystack
    g (code, plats) = let (point,u) = concretizeMicro wd code in (point, (u,plats))
    minsite = minimumBy (comparing (fst . snd)) (map h haystack)
    h (point, _) =
      let a = x
          b = x + v
          z = closestPointOnLine point a b
          dd = norm (point - z)
          ddd = norm (point - x)
      in (point, (dd,ddd))
    f (point, (u,plats)) =
      let a = x
          b = x + v
          z = closestPointOnLine point a b
          dt = norm (x - point) / norm v
          dd = norm (point - x)
      in if dd < 0.001 && not (leavingMicroSite v u) then [At dt point ((u,0), plats)] else []

-- data Env a = Env
--  { envMacroScan :: Float2 -> Float  -> [(Elem,a)]
--  , envMicroScan :: Float2 -> Float2 -> [At (Micro,[a])] } -- see notes on microscanning

{-
makeEnv :: Float -> AbstractLevel -> Env Plat
makeEnv padx (AL w h blocks) = Env elscan uscan where
  a = F2 (-160) (-240)
  b = F2 (-160) 240
  c = F2 160 240
  d = F2 160 (-240)
  elscan x r =
    [(RuleAB a b, Wall),(RuleAB b c, Wall)
    ,(RuleAB c d, Wall),(RuleAB d a, Wall)] :: [(Elem,Plat)]
  uscan x v =
    let myX = F2 160 240
        a = x
        b = x + v
        z = closestPointOnLine myX a b
        dt = norm (x - myX) / norm v
    in if norm (myX - z) < 0.001 && not (leavingMicroSite v corn)
      then [At dt myX (corn,[Wall])]
      else []
        where corn = Corner (F2 (-1) 0) (F2 0 (-1))
-}




-- instantly sweep a particle if its in the way, returns virtual time of collision
sweep :: Elem -> Float2 -> Float -> Particle -> Maybe (Float, Particle)
sweep el ev dt (P x v) = fmap f (intersectElement x (-ev) el) where
  f (dt', hitX) =
    let dt'' = dt - dt'
        x' = x + ev .* dt''
        n = normalToElementAt el hitX
        v' = reflect n v
        extra = 1
    in (dt', P x' v')
