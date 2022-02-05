{-# LANGUAGE NegativeLiterals #-}
module Numerals where


import Common
import Shape (Color(..), Sh(..))
import Debug.Trace

type Shape  = Sh Bool
type Cutout = Sh (Maybe Color)

-- make shapes for each of 16 or 17 numerals

ball = Ball
minus = Minus
union = Union
shrink :: Float -> Shape -> Shape
shrink x = Xform (F4 (1/x) 0 0 (1/x))

expand x = Xform (F4 x 0 0 x)

rotate a (F2 x y) = F2 x' y' where
  x' =  x * cos a + y * sin a
  y' = -x * sin a + y * cos a


trotate a (p1,p2,p3) = (p1',p2',p3') where
  p1' = rotate a p1
  p2' = rotate a p2
  p3' = rotate a p3

triangle = Trigon p1 p2 p3 where
  p1 = F2 -0.5 -0.5
  p2 = F2 0 0.5
  p3 = F2 0.5 -0.5

empt3 =
  ( shrink u
  . shift 0 -0.1666
  . donut 0.625
  . shift 0 0.1666) triangle

square = Axigon (F4 -0.5 -0.5 1 1)
squarehole = donut (3/4) square

star :: Shape
star = adj $ foldr1 union (map f [a,b,c,d,e]) where
  za = 0.5
  zb = za * cos (ang / 4)
  zh = 2 * zb * sin ang
  zc = 2 * zb * cos ang
  zd = 2 * zc * sin (ang / 2) / sin ang
  ze = zd * sin ang
  zf = zd * cos ang
  x = zc - zf
  y = zh - za - ze
  p1 = F2 (-x) (-y)
  p2 = F2 0 za
  p3 = F2 x (-y)
  ang :: Float
  ang = 2*pi/5 -- 72 deg
  a = (p1,p2,p3)
  b = trotate (1*ang) a
  c = trotate (2*ang) a
  d = trotate (3*ang) a
  e = trotate (4*ang) a
  f (x,y,z) = Trigon x y z
  fudge = 2*za - zh
  adj = shrink (1.3 * 2*za / zh) . shift 0 (-fudge/2)

diamond :: Shape
diamond = g (f square) where
  f = let ang = pi/4 in Xform (F4 (cos ang) (sin ang) (-(sin ang)) (cos ang))
  g = Xform (F4 1.666 0 0 1)


shift :: Float -> Float -> Shape -> Shape
shift x y = Shift (F2 x y)

donut :: Float -> Shape -> Shape
donut factor s = s `minus` (shrink factor s)

small :: Shape -> Shape
small s = shrink (1/3) s

pair a b = c `union` d where
  c = shift (-1/2) 0 a
  d = shift ( 1/2) 0 b

triple a b c =
  union
    (shift (-1/2) (-1/2) a)
    (union
      (shift 0 (1/2) b)
      (shift ( 1/2) (-1/2) c))

quad a b c d =
  union
    (shift 0 (1/2) (pair a b))
    (shift 0 (-1/2) (pair c d))

u = 0.666

zero  = shrink u (donut (3/4) ball)
one   = shrink u ball
two   = pair one one
three = shrink u triangle
four  = shrink u square
five = shrink u star
six = pair three three
seven = pair (shrink u squarehole) four
eight = pair four four
nine = triple three three three
ten = pair five five
eleven = triple (shrink u squarehole) four four
twelve = triple four four four
thirteen = shrink u diamond
fourteen = let o = (shrink u squarehole)
           in quad o four four o
fifteen = triple five five five
--sixteen = quad five four three one
sixteen = quad four four four four


numerals =
  [expand 0.75 zero
  ,expand 0.75 one
  ,two
  ,expand 0.75 three
  ,expand 0.75 four
  ,expand 0.75 five
  ,six
  ,seven
  ,eight
  ,nine
  ,ten
  ,eleven
  ,twelve
  ,expand 0.75 thirteen
  ,fourteen
  ,fifteen
  ,sixteen
  ]
