module Geometry where

import Common

-- an extended geometric path on the plane, carries label and payload
data Elem = RuleAB Float2 Float2 | Circle Float2 Float
  deriving (Eq,Ord,Show,Read)

-- microsurface view, either 1 normal or 2, refers to elements involved
data Micro = Flat Float2 | Corner Float2 Float2
  deriving (Eq,Ord,Show,Read)

-- line cross x negative, you're "on the right"
-- x cross line negative, you're "on the left"
lineSign :: Float2 -> Float2 -> Float2 -> Float
lineSign x la lb =
  let d = lb - la in
  let p = x - la in
  signum (p `cross2` d)

inElementCollisionSpace :: Float2 -> Elem -> Bool
inElementCollisionSpace x (RuleAB la lb) = lineSign x la lb > 0
inElementCollisionSpace x (Circle c r) = norm (x - c) > r

closestPointOnLine :: Float2 -> Float2 -> Float2 -> Float2
closestPointOnLine x la lb = 
  let d = la - lb in
  let n = d ./ norm d in
  let s = (x - la) `dot` n in
  la + s *. n

distanceToLine :: Float2 -> Float2 -> Float2 -> Float
distanceToLine x la lb = let y = closestPointOnLine x la lb in norm (x - y)

-- x between la lb returns 0=la, 1=lb
lineSegmentParam x la lb =
  let xla = x - la in
  let lbla = lb - la in
  let bigL = norm lbla in
  let n = lbla ./ bigL in
  (xla `dot` n) / bigL

normalToElementAt (RuleAB la lb) _ = let F2 x y = lb - la in normalize (F2 y (-x))
normalToElementAt (Circle c r) x   = normalize (x - c)


{-
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
-}
  

{-
nearElement :: Float2 -> Elem -> Bool
nearElement x (Circle c r) = let d = norm (c - x) in r - 0.001 < d && d < r + 0.001
nearElement x (RuleAB a b) =
  distanceToLine x a b < 0.001 &&
    let s = lineSegmentParam x a b in 0 <= s && s <= 1
-}
