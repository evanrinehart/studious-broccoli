{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
module ShapeCompiler where

import Control.Monad.State
import Control.Monad.Writer
import Text.Printf

import Shape
import Common

import Data.Type.Equality

data CompileState = CS { csNextVar :: Int, csIndent :: Int }
  deriving (Show)

type Compile a = StateT CompileState (Writer (Endo String)) a

data Color4 = Color4 !Float !Float !Float !Float deriving (Eq,Show)

newtype Var a = Var String 

instance PrintfArg (Var a) where
  formatArg (Var x) fmt = (x ++)

instance Show (Var a) where
  show (Var name) = name

execCompile :: Compile a -> String
execCompile act =
  let wr = evalStateT act (CS 0 0) in
  let (Endo g) = execWriter wr in
  g ""

codeOut :: String -> Compile ()
codeOut code = do
  n <- gets csIndent
  let line = replicate (2*n) ' ' ++ code ++ "\n"
  tell (Endo (line ++))

codesOut :: [String] -> Compile ()
codesOut codes = codeOut (concat codes)

indent :: Compile ()
indent = modify (\s -> s { csIndent = csIndent s + 1 })

unindent :: Compile ()
unindent = modify (\s -> s { csIndent = csIndent s - 1 })

freshInt :: Compile Int
freshInt = do
  v <- gets csNextVar
  modify (\s -> s { csNextVar = csNextVar s + 1 })
  return v

fresh :: Compile String
fresh = fmap (('v':) . show) freshInt

copy :: Var a -> Compile (Var a)
copy _ = do
  name <- fresh
  return (Var name)

tmpFor :: Sh a -> Compile (Var (GPUType a))
tmpFor _ = do
  name <- fresh
  return (Var name)

fragOut :: Var Color4
fragOut = Var "glFragColor"

type BoolOp = Var Bool -> Var Bool -> String

compileBoolOp :: BoolOp -> Sh Bool -> Sh Bool -> Var Float2 -> Var Bool -> Compile ()
compileBoolOp op s1 s2 invar outvar = do
  tmp1 <- tmpFor s1
  tmp2 <- tmpFor s2
  codeOut (printf "bool %s;" tmp1)
  codeOut (printf "bool %s;" tmp2)
  compile s1 invar tmp1
  compile s2 invar tmp2
  codeOut (printf "%s = %s;" outvar (op tmp1 tmp2))

compileShift :: Sample a => Float2 -> Sh a -> Var Float2 -> Var (GPUType a) -> Compile ()
compileShift (F2 dx dy) sh invar outvar = do
  tmp <- copy invar
  codeOut $ printf "%s %s = %s + vec2(%f,%f);" (tyName tmp) tmp invar dx dy
  compile sh tmp outvar

compileXform :: Sample a => Float4 -> Sh a -> Var Float2 -> Var (GPUType a) -> Compile ()
compileXform (F4 x y w h) sh invar outvar = do
  tmp <- copy invar
  codeOut $ printf "%s %s = %s * mat2(vec2(%f,%f),vec2(%f,%f));" (tyName tmp) tmp invar x y w h
  compile sh tmp outvar

compileBGColor :: Color -> Sh (Maybe Color) -> Var Float2 -> Var Color4 -> Compile ()
compileBGColor (C r g b) sh invar outvar = do
  -- case run sh of Nothing -> bg; Just c -> c
  tmp <- tmpFor sh
  codeOut (printf "vec4 %s;" tmp)
  compile sh invar tmp
  codeOut (printf "%s = %s.a > 0 ? %s : vec4(%f,%f,%f,1.0);" outvar tmp tmp r g b)

compileColorize :: Color -> Sh Bool -> Var Float2 -> Var Color4 -> Compile ()
compileColorize (C r g b) sh invar outvar = do
  -- if run sh then Just c else Nothing 
  tmp <- tmpFor sh
  codeOut (printf "bool %s;" tmp)
  compile sh invar tmp
  codeOut (printf "%s = vec4(%f,%f,%f, %s ? 1.0 : 0.0);" outvar r g b tmp)

compileLayer :: Sh (Maybe Color) -> Sh (Maybe Color) -> Var Float2 -> Var Color4 -> Compile ()
compileLayer sh1 sh2 invar outvar = do
  tmp <- tmpFor sh1
  codeOut (printf "vec4 %s;" tmp)
  -- run code for sh1 first, if visible, skip sh2
  compile sh1 invar tmp
  codeOut (printf "if(%s.a > 0){" tmp)
  codeOut (printf "  %s = %s;" outvar tmp)
  codeOut "} else {"
  indent
  compile sh2 invar outvar
  unindent
  codeOut "}"

compileBall :: Var Float2 -> Var Bool -> Compile ()
compileBall invar outvar = do
  codeOut $ printf "%s = ball(%s);" outvar invar

compileAxigon :: Float4 -> Var Float2 -> Var Bool -> Compile ()
compileAxigon (F4 x y w h) invar outvar = do
  codeOut $ printf "%s = axigon(%f,%f,%f,%f,%s);" outvar x y w h invar

compileTrigon :: Float2 -> Float2 -> Float2 -> Var Float2 -> Var Bool -> Compile ()
compileTrigon p1 p2 p3 invar outvar = do
  let fmt (F2 x y) = printf "vec2(%f,%f)" x y :: String
  let v1 = fmt p1
  let v2 = fmt p2
  let v3 = fmt p3
  codeOut $ printf "%s = trigon(%s,%s,%s,%s);" outvar v1 v2 v3 invar

compileCurve2 :: String -> Float2 -> Float2 -> Float2 -> Var Float2 -> Var Bool -> Compile ()
compileCurve2 suffix p1 p2 p3 invar outvar = do
  let fmt (F2 x y) = printf "vec2(%f,%f)" x y :: String
  let v1 = fmt p1
  let v2 = fmt p2
  let v3 = fmt p3
  codeOut $ printf "%s = curve2%s(%s,%s,%s,%s);" outvar suffix v1 v2 v3 invar

infactLayerOnlyWorksOnColors :: Sh (Maybe a) -> a :~: Color
infactLayerOnlyWorksOnColors sh = f sh where
  f :: Sh (Maybe a) -> a :~: Color
  f (Colorize c s) = Refl
  f (Layer s1 s2)  = f s1
  f (Xform _ s)  = f s
  f (Shift _ s)  = f s

compile :: Sample a => Sh a -> Var Float2 -> Var (GPUType a) -> Compile ()
compile sh invar outvar = f sh where
  f Ball = compileBall invar outvar
  f (Axigon xywh) = compileAxigon xywh invar outvar
  f (Trigon p1 p2 p3) = compileTrigon p1 p2 p3 invar outvar
  f (Curve2I p1 p2 p3) = compileCurve2 "in" p1 p2 p3 invar outvar
  f (Curve2O p1 p2 p3) = compileCurve2 "out" p1 p2 p3 invar outvar
  f (Union s1 s2) = compileBoolOp (printf "%s || %s")  s1 s2 invar outvar
  f (Inter s1 s2) = compileBoolOp (printf "%s || %s")  s1 s2 invar outvar
  f (Minus s1 s2) = compileBoolOp (printf "%s && !%s") s1 s2 invar outvar
  f (Colorize c s) = compileColorize c s invar outvar
  f l@(Layer s1 s2) = case infactLayerOnlyWorksOnColors l of
    Refl -> compileLayer s1 s2 invar outvar
  f (Shift d s) = compileShift d s invar outvar
  f (Xform m s) = compileXform m s invar outvar
  f (BGColor bg s) = compileBGColor bg s invar outvar

class Sample a where
  type GPUType a
  tyName :: Var a -> String

instance Sample Bool where
  type GPUType Bool = Bool
  tyName _ = "bool"

instance Sample Color where
  type GPUType Color = Color4
  tyName _ = "vec4"

instance Sample (Maybe Color) where
  type GPUType (Maybe Color) = Color4
  tyName _ = "vec4"

instance Sample Float2 where
  type GPUType Float2 = Float2
  tyName _ = "vec2"


crlf :: Compile ()
crlf = codeOut ""
  
toFrag :: Sh Color -> String
toFrag sh = execCompile $ do
  codeOut "#version 150"
  crlf
  codeOut "in vec2 uv;"
  codeOut "in vec2 pixelWH;"
  codeOut "out vec4 outColor;"
  crlf
  libraryOut codeForInside
  libraryOut codeForBarycentric
  libraryOut codeForTribox
  libraryOut codeForBall
  libraryOut codeForAxigon
  libraryOut codeForTrigon
  libraryOut (codeForCurve2 "in"  "<")
  libraryOut (codeForCurve2 "out" ">")
  crlf
  codeOut "void meat(in vec2 uv, out vec4 sample){"
  let invar  = Var "uv"
  let outvar = Var "sample"
  indent
  compile sh invar outvar
  unindent
  codeOut "}"
  crlf
  codeOut "void main(){"
  codeOut "  vec4 c0, c1, c2, c3;"
  codeOut "  float dx = pixelWH.x / 4;"
  codeOut "  float dy = pixelWH.y / 4;"
  --codeOut "  vec2 cent = vec2(0,0);"
  codeOut "  vec2 cent = pixelWH / 2;"
  codeOut "  meat(uv + cent + vec2(-dx,dy), c0);"
  codeOut "  meat(uv + cent + vec2(dx,dy), c1);"
  codeOut "  meat(uv + cent + vec2(-dx,-dy), c2);"
  codeOut "  meat(uv + cent + vec2(dx,-dy), c3);"
  codeOut "  float gamma = 2.2;"
  codeOut "  float r = (pow(c0.r, gamma) + pow(c1.r, gamma) + pow(c2.r, gamma) + pow(c3.r, gamma)) / 4;"
  codeOut "  float g = (pow(c0.g, gamma) + pow(c1.g, gamma) + pow(c2.g, gamma) + pow(c3.g, gamma)) / 4;"
  codeOut "  float b = (pow(c0.b, gamma) + pow(c1.b, gamma) + pow(c2.b, gamma) + pow(c3.b, gamma)) / 4;"
  codeOut "  outColor = vec4(pow(r, 1/gamma), pow(g, 1/gamma), pow(b, 1/gamma), 1);"
  codeOut "}"

libraryOut :: String -> Compile ()
libraryOut code = do
  tell (Endo (code ++))
  crlf


codeForInside = unlines
  ["bool inside(vec4 bb, vec2 uv){"
  ,"  float x1 = bb.x;"
  ,"  float x2 = bb.y;"
  ,"  float y1 = bb.z;"
  ,"  float y2 = bb.w;"
  ,"  return uv.x >= x1 && uv.x <= x2 && uv.y >= y1 && uv.y <= y2;"
  ,"}"]

codeForBarycentric = unlines
  ["vec3 barycentric(vec2 p1, vec2 p2, vec2 p3, vec2 uv){"
  ,"  float x1 = p1.x;"
  ,"  float y1 = p1.y;"
  ,"  float x2 = p2.x;"
  ,"  float y2 = p2.y;"
  ,"  float x3 = p3.x;"
  ,"  float y3 = p3.y;"
  ,"  float d = (y2 - y3)*(x1 - x3) + (x3 - x2)*(y1 - y3);"
  ,"  float x = uv.x;"
  ,"  float y = uv.y;"
  ,"  float n1 = (y2 - y3)*(x  - x3) + (x3 - x2)*(y  - y3);"
  ,"  float n2 = (y3 - y1)*(x  - x3) + (x1 - x3)*(y  - y3);"
  ,"  float w1 = n1 / d;"
  ,"  float w2 = n2 / d;"
  ,"  float w3 = 1 - w1 - w2;"
  ,"  return vec3(w1,w2,w3);"
  ,"}"]

codeForTribox = unlines
  ["vec4 tribox(vec2 p1, vec2 p2, vec2 p3){"
  ,"  float x1 = p1.x;"
  ,"  float y1 = p1.y;"
  ,"  float x2 = p2.x;"
  ,"  float y2 = p2.y;"
  ,"  float x3 = p3.x;"
  ,"  float y3 = p3.y;"
  ,"  float bx1 = min(x1,min(x2,x3));"
  ,"  float by1 = min(y1,min(y2,y3));"
  ,"  float bx2 = max(x1,max(x2,x3));"
  ,"  float by2 = max(y1,max(y2,y3));"
  ,"  return vec4(bx1,bx2,by1,by2);"
  ,"}"]

codeForBall = unlines
  ["bool ball(vec2 uv){"
  ,"  float x = uv.x;"
  ,"  float y = uv.y;"
  ,"  return x*x + y*y < 0.25;"
  ,"}"]

codeForAxigon = unlines
  ["bool axigon(float bx, float by, float bw, float bh, vec2 uv){"
  ,"  return inside(vec4(bx,bx+bw,by,by+bh),uv);"
  ,"}"]

codeForTrigon = unlines
  ["bool trigon(vec2 p1, vec2 p2, vec2 p3, vec2 uv){"

  ,"  vec4 bb = tribox(p1,p2,p3);"
  ,"  if(inside(bb,uv)){"
  ,"    vec3 w = barycentric(p1,p2,p3,uv);"
  ,"    return w.x >= 0 && w.y >= 0 && w.z >= 0;"
  ,"  }"

  ,"  return false;"
  ,"}"]


codeForCurve2 name op = unlines
  ["bool curve2" ++ name ++ "(vec2 p1, vec2 p2, vec2 p3, vec2 uv){"

  ,"  vec4 bb = tribox(p1,p2,p3);"
  ,"  if(!inside(bb,uv)) return false;"

  ,"  vec3 w = barycentric(p1,p2,p3,uv);"
  ,"  if(w.x < 0 || w.y < 0 || w.z < 0) return false;"

  ,"  float u = -w.x + w.z;"
  ,"  float v = w.y;"

  ,"  return v "++op++" (1 - u*u) / 2;"
  ,"}"]

