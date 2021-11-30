{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module TextMode where

import System.Exit

import Graphics.GL
import Codec.Picture

import Common
import Glue
import Paint
import HList

-- low level code to put glyphs on an off-screen texture

type PreBurn = Float2 -> BurnGlyph -- requires size of surface
type BurnGlyph = Glyph -> Float3 -> Float3 -> Int -> Int -> IO ()
type TextTool = (BurnGlyph -> IO ()) -> IO ()

type UL2 =
  ['("winWH",  Float2)
  ,'("srcXY",   Float2)
  ,'("srcWH",   Float2)
  ,'("dstXY",   Float2)
  ,'("dstWH",   Float2)
  ,'("fgColor", Float3)
  ,'("bgColor", Float3)]

{-
data ULegend2 = UL2
  { ul2SurfWH :: GLint
  , ul2SrcXY :: GLint
  , ul2SrcWH :: GLint
  , ul2DstXY :: GLint
  , ul2DstWH :: GLint
  , ul2FG    :: GLint
  , ul2BG    :: GLint }
-}

-- the computes source and destination rectangles for a glyph
glyphSrc :: Int -> Rect Float
glyphSrc c = Rect (fi sx) (fi sy) (fi w) (fi h) where
  w = 7
  h = 13
  sx = 2 * w * (c `mod` 40) + 42
  sy = h * (c `div` 40) + 42

glyphDst :: Int -> Int -> Rect Float
glyphDst i j = Rect (fi x) (fi y) (fi w) (fi h) where
  w = 7
  h = 13
  x = i * w
  y = magicH - (j + 1) * h

loadStraightShader :: VBO -> IO (Paint UL1)
loadStraightShader vbo = do
  vao <- newVAO
  useVAO vao

  useVBO vbo

  let name1 = "basic.vs"
  let name2 = "basic.fs"
  code1 <- readFile name1
  code2 <- readFile name2
  shader <- newShader name1 code1 name2 code2
  configAttrib shader "position" 2 8 0 GL_FLOAT
  let stones = ug2f . ug2f . ug2f . ug2f . ug2f
  gate <- buildGate shader (stones capstone)
{-
  ul0 <- getUniformLocation shader "winWH"
  ul1 <- getUniformLocation shader "srcXY"
  ul2 <- getUniformLocation shader "srcWH"
  ul3 <- getUniformLocation shader "dstXY"
  ul4 <- getUniformLocation shader "dstWH"
-}

  return (Paint vao shader gate)

loadGlyphShader :: VBO -> IO (Paint UL2)
loadGlyphShader vbo = do
  vao <- newVAO
  useVAO vao

  useVBO vbo

  let name1 = "basic.vs"
  let name2 = "glyph.fs"
  code1 <- readFile name1
  code2 <- readFile name2
  shader <- newShader name1 code1 name2 code2
  configAttrib shader "position" 2 8 0 GL_FLOAT

  let stones = ug2f . ug2f . ug2f . ug2f . ug2f . ug3f . ug3f
  gate <- buildGate shader (stones capstone)

  {-
  ul0 <- getUniformLocation shader "winWH"
  ul1 <- getUniformLocation shader "srcXY"
  ul2 <- getUniformLocation shader "srcWH"
  ul3 <- getUniformLocation shader "dstXY"
  ul4 <- getUniformLocation shader "dstWH"
  ul5 <- getUniformLocation shader "fgColor"
  ul6 <- getUniformLocation shader "bgColor"
  -}

  putStrLn "text mode set up"
  assertGL "text mode code"

  return (Paint vao shader gate)

newTextSurface :: IO (FBO,Tex)
newTextSurface = do
  let w = magicW
  let h = magicH
  fbo <- newFBO
  useFBO fbo
  surf <- newBlankTexture w h
  useTexture surf
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
  attachTex surf
  status <- glCheckFramebufferStatus GL_FRAMEBUFFER
  if status /= GL_FRAMEBUFFER_COMPLETE
    then putStrLn "framebuffer incomplete"
    else return ()
  useFBO (FBO 0)
  return (fbo,surf)

newTextTool :: VBO -> Tex -> Paint UL2 -> FBO -> TextTool
newTextTool vbo tex (Paint vao shader uniforms) fbo action = do
  useVAO vao
  useVBO vbo
  useShader shader
  useTexture tex -- note, this is the font not the drawing surface
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
  useFBO fbo
  glViewport 0 0 magicW magicH
  action (putGlyph uniforms (F2 magicW magicH))
  glViewport 0 0 800 600
  useFBO (FBO 0)
  assertGL "yoooo"

putGlyph :: Gate UL2 -> Float2 -> Glyph -> Float3 -> Float3 -> Int -> Int -> IO ()
putGlyph uniforms winWH (Glyph c) fg bg i j = do
  let F3 r1 g1 b1 = fg
  let F3 r2 g2 b2 = bg
  let Rect sx sy sw sh = glyphSrc c
  let Rect dx dy dw dh = glyphDst i j
  configUniforms uniforms $
    Field @"winWH" winWH >:
    Field @"srcXY" (F2 sx sy) >:
    Field @"srcWH" (F2 sw sh) >:
    Field @"dstXY" (F2 dx dy) >:
    Field @"dstWH" (F2 dw dh) >:
    Field @"fgColor" fg >:
    Field @"bgColor" bg >:
    R0
  renderQuad

slap :: VBO -> Paint UL1 -> Rect Float -> Tex -> IO ()
slap vbo (Paint vao shader uniforms) (Rect x y w h) tex = do
  useVAO vao
  useVBO vbo
  useShader shader
  useTexture tex
  configUniforms uniforms $
    Field @"winWH" (F2 800 600) >:
    Field @"srcXY" (F2 0 0) >:
    Field @"srcWH" (F2 w h) >:
    Field @"dstXY" (F2 x y) >:
    Field @"dstWH" (F2 w h) >:
    R0
  renderQuad

magicW :: Num a => a
magicW = 800

magicH :: Num a => a
magicH = 600
