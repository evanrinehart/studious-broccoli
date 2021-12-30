module Geometry where

import Data.List (minimumBy)
import Data.Ord (comparing)

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
  let xla  = x - la in
  let lbla = lb - la in
  let bigL = norm lbla in
  let n = lbla ./ bigL in
  (xla `dot` n) / bigL

lineNormal la lb = let F2 x y = lb - la in normalize (F2 y (-x))

normalToElementAt (RuleAB la lb) _ = let F2 x y = lb - la in normalize (F2 y (-x))
normalToElementAt (Circle c r) x   = normalize (x - c)

lineDepth :: Float2 -> Float2 -> Float2 -> (Float, Float2)
lineDepth x a b =
  let d = closestPointOnLine x a b - x
  in (norm d * lineSign x a b, d)

circleDepth :: Float2 -> Float2 -> Float -> (Float, Float2)
circleDepth x c r =
  let d = x - c
      s = norm d
      depth = r - s
      danger = s < 0.001 in
  if danger
    then (-r, F2 r 0)
    else (-depth, (d ./ s) .* depth)

data RC = RC
  { rcRadius :: Float
  , rcCenter :: Float2
  , rcN1 :: Float2
  , rcN2 :: Float2 }
      deriving Show

rcLine1 :: RC -> (Float2, Float2)
rcLine1 (RC r c n1 _) = (a, b) where
  a = c + n1 .* r
  b = a - lineNormal c a

rcLine2 :: RC -> (Float2, Float2)
rcLine2 (RC r c _ n2) = (a, b) where
  a = c + n2 .* r
  b = a - lineNormal c a

hardCornerDepth :: Float2 -> RC -> Maybe Float2
hardCornerDepth x rc =
  let (a1,b1) = rcLine1 rc
      (a2,b2) = rcLine2 rc
      (s1,d1) = lineDepth x a1 b1
      (s2,d2) = lineDepth x a2 b2
  in if s1 < 0 && s2 < 0
    then Just (minimumBy (comparing norm) [d1,d2])
    else Nothing

roundedCornerDepth :: Float2 -> RC -> Maybe Float2
roundedCornerDepth x rc = 
  let (a1,b1) = rcLine1 rc
      (a2,b2) = rcLine2 rc
      (s1,d1) = lineDepth x a1 b1
      (s2,d2) = lineDepth x a2 b2
      RC r c _ _ = rc in
  if s1 < 0 && s2 < 0
    then if -s1 < r && -s2 < r
      then let (s3,d3) = circleDepth x c r in
        if s3 < 0
          then Just d3
          else Nothing
      else Just (minimumBy (comparing norm) [d1,d2])
    else Nothing

depthWithinHull :: Float2 -> [RC] -> Maybe Float2
depthWithinHull x []  = error "depthWithinHull: no hull"
depthWithinHull x rcs = fmap (minimumBy (comparing norm)) (mapM (roundedCornerDepth x) rcs)

ex = [rc1, rc2, rc3, rc4] where
  neg1 = -1
  rc1 = RC 1 (F2 1 1) (F2 0 1) (F2 1 0)
  rc2 = RC 1 (F2 neg1 1) (F2 0 1) (F2 neg1 0)
  rc3 = RC 1 (F2 1 neg1) (F2 0 neg1) (F2 1 0)
  rc4 = RC 1 (F2 neg1 neg1) (F2 0 neg1) (F2 neg1 0)


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
