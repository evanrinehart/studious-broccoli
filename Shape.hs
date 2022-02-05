{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module Shape where

import Data.List (foldl', intercalate)
import Data.Maybe
import Control.Applicative

import Codec.Picture as JuicyPixels

import Common hiding (I2)
import Geometry

import Debug.Trace

import Dump

-- a shape, for the time being, is a function from the plane to Bool.
-- a simple way to "destruct" a shape is to render it within a given rectangular region
-- at a given resolution. To form the primitive shapes though we need...

-- simple shapes:
-- the unit disc - simply true if and only if r < 1
-- the unit tile - true if xy in [-1/2, +1/2]^2
-- yeah the unit tile is half the size of the unit disc
-- "the triangle", (0,0) (1,0) (1/2, sin(60)) 
-- can be centered by shift (-1/2, -tan(30)/2)
-- also smaller than the disc... maybe we should shrink the disc

data I1 = I1 !Float !Float deriving Show
data I2 = I2 !I1 !I1 deriving Show

type Shape a = Float2 -> a
data Color = C !Float !Float !Float deriving (Show,Read)

encodeColor :: Color -> PixelRGB8
encodeColor (C r g b) = PixelRGB8 r' g' b' where
  r' = floor (r * 255)
  g' = floor (g * 255)
  b' = floor (b * 255)

mix1 :: Float -> Float -> Float -> Float
mix1 q x y = x*(1 - q) + y*q

mixColor :: Float -> Color -> Color -> Color
mixColor q (C r1 g1 b1) (C r2 g2 b2) =
  C (mix1 q r1 r2) (mix1 q g1 g2) (mix1 q b1 b2)

disc :: Shape Bool
disc (F2 x y) = (x*x + y*y) < 0.25

block :: Shape Bool
block (F2 x y) =
  x > -0.5 && x < 0.5 && y > -0.5 && y < 0.5
  
triangle :: Shape Bool
triangle (F2 x y) =
  let m = tan (pi / 3)
      b = m / 4 
      xx = x + 1/2
  in y > -b && y < m * xx - b && y < -m * xx + (m - b)

-- the parabolic area within triangle (-1,0) (0,1) (1,0)
parab1 :: Shape Bool
parab1 (F2 x y) =
  y > 0 &&
  y < x + 1 && 
  y < 1 - x &&
  y < (1 - x*x) / 2

parab2 :: Shape Bool
parab2 (F2 x y) =
  y > 0 &&
  y < x + 1 && 
  y < 1 - x &&
  y > (1 - x*x) / 2

barycentric (F2 x1 y1) (F2 x2 y2) (F2 x3 y3) (F2 x y) =
  let d  = (y2 - y3)*(x1 - x3) + (x3 - x2)*(y1 - y3)
      n1 = (y2 - y3)*(x  - x3) + (x3 - x2)*(y  - y3)
      n2 = (y3 - y1)*(x  - x3) + (x1 - x3)*(y  - y3)
      w1 = n1 / d
      w2 = n2 / d
      w3 = 1 - w1 - w2
  in F3 w1 w2 w3

tribox :: Float2 -> Float2 -> Float2 -> Float4
tribox (F2 x1 y1) (F2 x2 y2) (F2 x3 y3) =
  let xmin = min x1 (min x2 x3)
      ymin = min y1 (min y2 y3)
      xmax = max x1 (max x2 x3)
      ymax = max y1 (max y2 y3)
  in F4 xmin xmax ymin ymax

arbtri :: Float2 -> Float2 -> Float2 -> Shape Bool
arbtri p1 p2 p3 xy@(F2 x y) = 
  let F3 w1 w2 w3 = barycentric p1 p2 p3 xy
      u = -w1 + w3
      v = w2
      F4 a b c d = tribox p1 p2 p3
      wayout = x < a || y < c || x > b || y > d
  in not wayout && w1 > 0 && w2 > 0 && w3 > 0

arbbox :: Float4 -> Shape Bool
arbbox (F4 bx by bw bh) (F2 x y) =
  x > bx &&
  x < bx + bw &&
  y > by &&
  y < by + bh
      

-- a parabolic knee contained in a triangle. The curve begins
-- at p1 and ends at p3. p2 is a control point. The curve forms
-- a tangent with the triangle at the beginning and end.
curve2 :: Float2 -> Float2 -> Float2 -> Shape Bool
curve2 p1 p2 p3 xy@(F2 x y) =
  let F3 w1 w2 w3 = barycentric p1 p2 p3 xy
      u = -w1 + w3
      v = w2 in
  if w1 < 0 || w2 < 0 || w3 < 0
    then False
    else v < (1 - u*u) / 2

curve2_ :: Float2 -> Float2 -> Float2 -> Shape Bool
curve2_ p1 p2 p3 xy@(F2 x y) =
  let F3 w1 w2 w3 = barycentric p1 p2 p3 xy
      u = -w1 + w3
      v = w2 in
  if w1 < 0 || w2 < 0 || w3 < 0
    then False
    else v > (1 - u*u) / 2
      

--theshape = (rotate (pi) . shift (F2 0.5 0) . xform (F4 2 1 0 -2)) parab
--theshape = curve2_ (F2 0 -1) (F2 0 1) (F2 -1 0)

bcode :: Shape Bool
bcode =
  let stock = arbbox (F4 -0.5 -0.5 1 1)
      cliff = 0.3
      cut1 = curve2_ (F2 cliff 0.5) (F2 0.5 0.5) (F2 0.5 0.25)
      cut2 = curve2_ (F2 0.5 0.25) (F2 0.5 0) (F2 cliff 0)
      cut3 = xform (F4 1 0 0 -1) cut2
      cut4 = xform (F4 1 0 0 -1) cut1
      cut5 = arbbox (F4 -0.5 -0.4 0.1 0.8)
      cut6 = let aux = arbbox (F4 -0.3 -0.4 0.7 0.35)
             in foldl minus aux
                  [curve2_ (F2 -0.3 -0.05) (F2 0.4 0.05) (F2 0.4 -0.2)
                  ,curve2_ (F2 0.3 -0.4) (F2 0.4 -0.4) (F2 0.4 -0.3)]
      cut7 = xform (F4 1 0 0 -1) cut6
      cuts = [cut1,cut2,cut3,cut4,cut5,cut6,cut7]
  in foldl minus stock cuts

bdata :: Sh Bool
bdata =
  let stock = Axigon (F4 -0.5 -0.5 1 1)
      cliff = 0.3
      cut1 = Curve2O (F2 cliff 0.5) (F2 0.5 0.5) (F2 0.5 0.25)
      cut2 = Curve2O (F2 0.5 0.25) (F2 0.5 0) (F2 cliff 0)
      cut3 = Xform (F4 1 0 0 -1) cut2
      cut4 = Xform (F4 1 0 0 -1) cut1
      cut5 = Axigon (F4 -0.5 -0.4 0.1 0.8)
      cut6 = let aux = Axigon (F4 -0.3 -0.4 0.7 0.35)
             in foldl Minus aux
                  [Curve2O (F2 -0.3 -0.05) (F2 0.4 0.05) (F2 0.4 -0.2)
                  ,Curve2O (F2 0.3 -0.4) (F2 0.4 -0.4) (F2 0.4 -0.3)]
      cut7 = Xform (F4 1 0 0 -1) cut6
      cuts = [cut1,cut2,cut3,cut4,cut5,cut6,cut7]
  in foldl Minus stock cuts


fun :: (Float -> Float) -> Shape Bool
fun f (F2 x y) = if y < f x then True else False

minus :: Shape Bool -> Shape Bool -> Shape Bool
minus f g = pure h <*> f <*> g where
  h _ True  = False
  h x False = x

union f g = pure (||) <*> f <*> g

shift :: Float2 -> Shape a -> Shape a
shift (F2 dx dy) f (F2 x y) = f (F2 (x-dx) (y-dy))

rotate :: Float -> Shape a -> Shape a
rotate angle f (F2 x y) = f (F2 x' y') where
  x' =  x * cos angle + y * sin angle
  y' = -x * sin angle + y * cos angle

xform :: Float4 -> Shape a -> Shape a
xform (F4 a b c d) f (F2 x y) = f (F2 x' y') where
  x' = x * a + y * b
  y' = x * c + y * d

everything :: Shape Bool
everything = const True

nothing :: Shape Bool
nothing = const False

half :: Float2 -> Shape Bool
half (F2 n1 n2) xy = lineSign xy (F2 0 0) (F2 n2 (-n1)) > 0

colorize :: Color -> Shape Bool -> Shape (Maybe Color)
colorize c = fmap g where
  g False = Nothing
  g True  = Just c

layer :: Shape (Maybe a) -> Shape (Maybe a) -> Shape (Maybe a)
layer f g = pure h <*> f <*> g where
  h Nothing Nothing  = Nothing
  h Nothing (Just y) = Just y
  h (Just x) _       = Just x

bgcolor :: Color -> Shape (Maybe Color) -> Shape Color
bgcolor bg = fmap (fromMaybe bg)


inter :: Shape Bool -> Shape Bool -> Shape Bool
inter f g = pure (&&) <*> f <*> g

{-
theshape :: Shape Bool
--theshape = xform (F4 1 0 0 1) $ 
theshape = rotate 0.2 $ 
  let cut xy n = shift xy (half n)
  in cut (F2 0.47 0) (F2 1 0) `inter`
     cut (F2 -0.47 0) (F2 -1 0) `inter`
     cut (F2 0.25 0.4) (F2 0.5 1) `inter`
     cut (F2 -0.25 -0.4) (F2 -0.5 -1) `inter`
     cut (F2 -0.25 0.4) (F2 -0.5 1) `inter`
     cut (F2 0.25 -0.4) (F2 0.5 -1) `inter`
     everything
-}




multisample :: ([a] -> b) -> Int -> I2 -> Shape a -> b
multisample avg n box f = (avg . map f) (raster n box) where

renderBW :: Int -> Int -> (Int -> Int -> I2) -> Shape Bool -> DynamicImage
renderBW w h win shape = ImageRGB8 (JuicyPixels.generateImage f w h) where
  f i j =
    let wt = multisample avg 2 (win i j) shape
    in encodeColor (C wt wt wt)
  avg xs = let ws = map weigh xs
           in sum ws / (fromIntegral (length ws))
  weigh False = 1.0
  weigh True  = 0.0

printBW :: I2 -> Int -> Int -> Shape Bool -> FilePath -> IO ()
printBW region imgw imgh shape path = do
  let img = renderBW imgw imgh (basicWindow region imgw imgh) shape
  JuicyPixels.savePngImage path img

renderRGB :: Int -> Int -> (Int -> Int -> I2) -> Shape Color -> DynamicImage
renderRGB w h win shape = ImageRGB8 (JuicyPixels.generateImage f w h) where
  f i j = encodeColor (multisample avgColor 1 (win i j) shape)
  avgColor cs =
    let rs = map (\(C r g b) -> ungamma r) cs
        gs = map (\(C r g b) -> ungamma g) cs
        bs = map (\(C r g b) -> ungamma b) cs
    in C (gamma $ avg rs) (gamma $ avg gs) (gamma $ avg bs)

gamma x   = x ** (1/2.2)
ungamma x = x ** 2.2


printRGB :: I2 -> Int -> Int -> Shape Color -> FilePath -> IO ()
printRGB region imgw imgh shape path = do
  let img = renderRGB imgw imgh (basicWindow region imgw imgh) shape
  JuicyPixels.savePngImage path img

avg :: [Float] -> Float
avg xs = total / size where
  (total,size) = foldl' v (0,0) xs
  v (!a, !l) x = (a + x, l + 1)

{-
renderRGB
  :: Int
  -> Int
  -> (Int -> Int -> I2)
  -> Shape (Maybe Color)
  -> Color
  -> DynamicImage
renderRGB w h win shape bg = ImageRGB8 (JuicyPixels.generateImage f w h) where
  bge = encodeColor bg
  f :: Int -> Int -> PixelRGB8
  f i j = case multisample 2 (win i j) (bgcolor bg shape) of
    Nothing -> bge
    Just (c,weight) -> encodeColor (mixColor weight bg c)
-}


basicWindow :: I2 -> Int -> Int -> Int -> Int -> I2
basicWindow (I2 (I1 a1 a2) (I1 b1 b2)) imgw imgh i j = 
  let s = (a2 - a1) / (fromIntegral imgw)
      xbase = fromIntegral i * s + a1
      ybase = b2 - fromIntegral j * s - s
  in I2 (I1 xbase (xbase + s)) (I1 ybase (ybase + s))

square :: I2
square = I2 (I1 0 1) (I1 0 1)

thequad :: I2
thequad = I2 (I1 -1 1) (I1 -1 1)

bisect :: I1 -> (I1,I1)
bisect (I1 a b) = let p = (a + b) / 2 in (I1 a p, I1 p b)

quadsect :: I2 -> [I2]
quadsect (I2 a b) = 
  let (a1,a2) = bisect a
      (b1,b2) = bisect b
  in [I2 a1 b1, I2 a1 b2, I2 a2 b1, I2 a2 b2]

midpoint1 :: I1 -> Float
midpoint1 (I1 a b) = (a + b) / 2

midpoint2 :: I2 -> Float2
midpoint2 (I2 a b) = F2 (midpoint1 a) (midpoint1 b)

preraster :: Int -> I2 -> [I2]
preraster lvl q = go lvl [q] where
  go 0 xs = xs
  go n xs = go (n - 1) (concatMap quadsect xs)

raster :: Int -> I2 -> [Float2]
raster n q = map midpoint2 (preraster n q)


{-
data Expr a where
  App  :: Expr (a -> b) -> Expr a -> Expr b
  Lam  :: Scope b -> Expr (a -> b)
  BVar :: Int -> String -> Expr a
  FVar :: String -> Expr a

newtype Scope a b = Scope (Expr b) deriving Show 
-}

data Sh a where
  Ball :: Sh Bool
  Axigon :: Float4 -> Sh Bool
  Trigon :: Float2 -> Float2 -> Float2 -> Sh Bool
  Curve2I :: Float2 -> Float2 -> Float2 -> Sh Bool
  Curve2O :: Float2 -> Float2 -> Float2 -> Sh Bool
  Union :: Sh Bool -> Sh Bool -> Sh Bool
  Inter :: Sh Bool -> Sh Bool -> Sh Bool
  Minus :: Sh Bool -> Sh Bool -> Sh Bool
  Colorize :: Color -> Sh Bool -> Sh (Maybe Color)
  Layer :: Sh (Maybe a) -> Sh (Maybe a) -> Sh (Maybe a)
  Shift :: Float2 -> Sh a -> Sh a
  Xform :: Float4 -> Sh a -> Sh a
  BGColor :: Color -> Sh (Maybe Color) -> Sh Color

deriving instance Show (Sh a)


blackOnWhite :: Sh Bool -> Sh Color
blackOnWhite = BGColor (C 1 1 1) . Colorize (C 0 0 0)


interpretSh :: Sh a -> Shape a
interpretSh sh = f sh where
  f :: Sh a -> Shape a
  f Ball = disc
  f (Axigon d) = arbbox d
  f (Trigon a b c) = arbtri a b c
  f (Curve2I a b c) = curve2 a b c
  f (Curve2O a b c) = curve2_ a b c
  f (Union s1 s2) = union (f s1) (f s2)
  f (Inter s1 s2) = inter (f s1) (f s2)
  f (Minus s1 s2) = minus (f s1) (f s2)
  f (Colorize c s) = colorize c (f s)
  f (Layer s1 s2) = layer (f s1) (f s2)
  f (Shift d s) = shift d (f s)
  f (Xform mat s) = xform mat (f s)
  f (BGColor c s) = bgcolor c (f s)

dumpShape :: Sh a -> String
dumpShape = dump . dumpableShape

data DynamicSh = ShBW (Sh Bool) | ShColor (Sh Color) | ShCutout (Sh (Maybe Color))
  deriving Show

undumpShape :: String -> Maybe DynamicSh
undumpShape = shapeFromDump . undump

shapeFromDump :: Dy -> Maybe DynamicSh
shapeFromDump (F _) = Nothing
shapeFromDump d = go d where
  go :: Dy -> Maybe DynamicSh
  go (D "Ball" []) = pure (ShBW Ball)
  go (D "Axigon" [F a, F b, F c, F d]) = pure (ShBW (Axigon (F4 a b c d)))
  go (D "Trigon" [F a1, F a2, F b1, F b2, F c1, F c2]) =
    pure (ShBW (Trigon (F2 a1 a2) (F2 b1 b2) (F2 c1 c2)))
  go (D "Curve2" [D "I" [], F a1, F a2, F b1, F b2, F c1, F c2]) =
    pure (ShBW (Curve2I (F2 a1 a2) (F2 b1 b2) (F2 c1 c2)))
  go (D "Curve2" [D "O" [], F a1, F a2, F b1, F b2, F c1, F c2]) =
    pure (ShBW (Curve2O (F2 a1 a2) (F2 b1 b2) (F2 c1 c2)))
  go (D "Union" [d1@(D _ _), d2@(D _ _)]) = do
    ShBW s1 <- go d1
    ShBW s2 <- go d2
    pure (ShBW (Union s1 s2))
  go (D "Inter" [d1@(D _ _), d2@(D _ _)]) = do
    ShBW s1 <- go d1
    ShBW s2 <- go d2
    pure (ShBW (Inter s1 s2))
  go (D "Minus" [d1@(D _ _), d2@(D _ _)]) = do
    ShBW s1 <- go d1
    ShBW s2 <- go d2
    pure (ShBW (Minus s1 s2))
  go (D "Layer" [d1@(D _ _), d2@(D _ _)]) = do
    ShCutout hmm1 <- go d1
    ShCutout hmm2 <- go d2
    pure (ShCutout (Layer hmm1 hmm2))
  go (D "Shift" [F dx, F dy, d@(D _ _)]) = do
    hmm <- go d
    case hmm of
      ShBW s     -> pure (ShBW (Shift (F2 dx dy) s))
      ShColor s  -> pure (ShColor (Shift (F2 dx dy) s))
      ShCutout s -> pure (ShCutout (Shift (F2 dx dy) s))
  go (D "Xform" [F a, F b, F c, F d, e@(D _ _)]) = do
    hmm <- go e
    let build :: Sh a -> Sh a; build s = Xform (F4 a b c d) s
    case hmm of
      ShBW s     -> (pure . ShBW . build) s
      ShColor s  -> (pure . ShColor . build) s
      ShCutout s -> (pure . ShCutout . build) s
  go (D "Colorize" [F r, F g, F b, d@(D _ _)]) = do
    ShBW s <- go d
    pure (ShCutout (Colorize (C r g b) s))
  go (D "BGColor" [F r, F g, F b, d@(D _ _)]) = do
    ShCutout s <- shapeFromDump d
    pure (ShColor (BGColor (C r g b) s))
  go _ = Nothing


dumpableShape :: Sh a -> Dy
dumpableShape sh = f sh where
  f :: Sh b -> Dy
  f Ball = D "Ball" []
  f (Axigon (F4 a b c d)) = D "Axigon" (map F [a,b,c,d])
  f (Trigon (F2 a b) (F2 c d) (F2 e f)) = D "Trigon" (map F [a,b,c,d,e,f])
  f (Curve2I (F2 a b) (F2 c d) (F2 e f)) = D "Curve2" (D "I" [] : map F [a,b,c,d,e,f])
  f (Curve2O (F2 a b) (F2 c d) (F2 e f)) = D "Curve2" (D "O" [] : map F [a,b,c,d,e,f])
  f (Union s1 s2) = D "Union" [f s1, f s2]
  f (Inter s1 s2) = D "Inter" [f s1, f s2]
  f (Minus s1 s2) = D "Minus" [f s1, f s2]
  f (Colorize (C r g b) s) = D "Colorize" [F r, F g, F b, f s]
  f (Layer s1 s2) = D "Layer" [f s1, f s2]
  f (Shift (F2 dx dy) s) = D "Shift" [F dx, F dy, f s]
  f (Xform (F4 a b c d) s) = D "Xform" [F a, F b, F c, F d, f s]
  f (BGColor (C r g b) s) = D "BGColor" [F r, F g, F b, f s]


{-

-- a boolean shape, R2 -> Bool
data ABC f where
  Ball   :: ABC f
  Axigon :: f Float4 -> ABC f
  Trigon :: f Float2 -> f Float2 -> f Float2 -> ABC f
  Curve2 :: InOut -> f Float2 -> f Float2 -> f Float2 -> ABC f
  BoolOp :: UIM -> f (ABC f) -> f (ABC f) -> ABC f
  Xform1 :: f Float33 -> f (ABC f) -> ABC f

-- a colored cutout, R2 -> Maybe Color
data BCD f where
  Colorize :: f Color   -> f (ABC f) -> BCD f
  Layer    :: f (BCD f) -> f (BCD f) -> BCD f
  Xform2   :: f Float33 -> f (BCD f) -> BCD f

-- a color picture, R2 -> Color
data CDE f where
  BGColor :: f Color   -> f (BCD f) -> CDE f
  Xform3  :: f Float33 -> f (CDE f) -> CDE f
  
-- f = Identity, all data present
-- f = Maybe, data is present or missing, needs to be filled in
-- f = LC, data is there, is a variable, or a lambda expression

data LC a where
  Val :: a -> LC a
  Lam :: String -> Scope a b -> LC (a -> b)
  App :: LC (a -> b) -> LC a -> LC b
  BV  :: Int -> LC a

data Scope a b = Scope (LC b) -- but contains a bound var of type a

-}
