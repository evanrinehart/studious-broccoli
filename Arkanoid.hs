{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
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



-- PARTICLE PHYSICS

data Particle = P !Float2 !Float2
  deriving Show

particlePosition :: Particle -> Float2
particlePosition (P x _) = x

propagateX dt x v = x + v .* dt
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

-- if you're on the line, dt=zero
particleHitsInfiniteLine :: Float2 -> Float2 -> Float2 -> Float2 -> Maybe (Float,Float2)
particleHitsInfiniteLine x v la lb =
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

particleHitsCircle :: Float2 -> Float2 -> Float2 -> Float -> Maybe (Float,Float2)
particleHitsCircle x v c r = case timeUntilCircle x v c r of
  Nothing -> Nothing
  Just (t1,t2)
    | t2 < 0    -> Nothing
    | t1 < 0    -> Just (t2, propagateX t2 x v)
    | otherwise -> Just (t1, propagateX t1 x v)

-- "one sided lines"
intersectElement :: Float2 -> Float2 -> Elem -> Maybe (Float,Float2)
intersectElement x v el =
  if not (inElementCollisionSpace x el)
    then Nothing
    else case el of
      RuleAB la lb -> particleHitsLine x v la lb
      Circle c r   -> particleHitsCircle x v c r

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

data Battle a = Battle Float Float2 Micro Int a 
  deriving Show

instance Ord (Battle a) where
  compare (Battle t1 _ _ lvl1 _) (Battle t2 _ _ lvl2 _) = compare t1 t2 <> compare lvl1 lvl2

instance Eq (Battle a) where
  b1 == b2 = case compare b1 b2 of
    EQ -> True
    _  -> False

battle :: Float -> [At (Elem,a)] -> [At (Micro,[a])] -> Maybe (At (Micro,[a]))
battle limit ehits uhits = 
  let mergingA = map f ehits
      mergingB = map g uhits
      f (At t x (el,z)) = Battle t x (Flat (normalToElementAt el x)) 1 [z]
      g (At t x (u,zs)) = Battle t x u 0 zs
      combatants = mergingA ++ mergingB
      Battle t x u _ zs = if null combatants
        then let o = undefined in Battle (1/0) o o o o
        else minimum combatants
  in if t <= limit
    then Just (At t x (u, zs))
    else Nothing

intersectElements :: Particle -> [(Elem,a)] -> [At (Elem,a)]
intersectElements (P x v) elements = 
  let g el z (t,p) = At t p (el, z)
      f (el,z) = fmap (g el z) (intersectElement x v el)
  in catMaybes (map f elements)


data At a = At
  { atT :: !Float
  , atX :: !Float2
  , atStuff :: a }
      deriving Show

data Env a = Env
  { envMacroScan :: Float2 -> Float  -> [(Elem,a)]
  , envMicroScan :: Float2 -> Float2 -> [At (Micro,[a])] } -- see notes on microscanning

particle :: Env a -> Float -> Particle -> Writer [At [a]] Particle
particle env limit p_initial = loop 0 p_initial where
  loop t p@(P x v) = do
    let radius = norm v * (limit - t)
    let elements = envMacroScan env x radius
    let hits = intersectElements p elements
    let microhits = envMicroScan env x v
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

debugParticle :: Show a => Env a -> Float -> Particle -> [DebugParticle]
debugParticle env limit p_initial = loop 0 p_initial where
  loop t p@(P x v) =
    let radius = norm v * (limit - t)
        elements = envMacroScan env x radius
        hits = intersectElements p elements
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
