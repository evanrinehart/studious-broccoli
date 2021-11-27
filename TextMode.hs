module TextMode where

import System.Exit

import Graphics.GL
import Codec.Picture

import Common
import Glue

-- low level code to put glyphs on an off-screen texture

type PreBurn = Float2 -> BurnGlyph -- requires size of surface
type BurnGlyph = Glyph -> Float3 -> Float3 -> Int -> Int -> IO ()
type TextTool = (BurnGlyph -> IO ()) -> IO ()

data ULegend2 = UL2
  { ul2SurfWH :: GLint
  , ul2SrcXY :: GLint
  , ul2SrcWH :: GLint
  , ul2DstXY :: GLint
  , ul2DstWH :: GLint
  , ul2FG    :: GLint
  , ul2BG    :: GLint }

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

loadStraightShader :: VBO -> IO (VAO,Shader,ULegend1)
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
  ul0 <- getUniformLocation shader "winWH"
  ul1 <- getUniformLocation shader "srcXY"
  ul2 <- getUniformLocation shader "srcWH"
  ul3 <- getUniformLocation shader "dstXY"
  ul4 <- getUniformLocation shader "dstWH"

  return (vao, shader, UL1 ul0 ul1 ul2 ul3 ul4)

loadGlyphShader :: VBO -> IO (VAO,Shader,ULegend2)
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
  ul0 <- getUniformLocation shader "winWH"
  ul1 <- getUniformLocation shader "srcXY"
  ul2 <- getUniformLocation shader "srcWH"
  ul3 <- getUniformLocation shader "dstXY"
  ul4 <- getUniformLocation shader "dstWH"
  ul5 <- getUniformLocation shader "fgColor"
  ul6 <- getUniformLocation shader "bgColor"

  putStrLn "text mode set up"
  assertGL "text mode code"

  return (vao, shader, UL2 ul0 ul1 ul2 ul3 ul4 ul5 ul6)

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

newTextTool :: VBO -> Tex -> (VAO,Shader,ULegend2) -> FBO -> TextTool
newTextTool vbo tex (vao,shader,legend) fbo action = do
  useVAO vao
  useVBO vbo
  useShader shader
  useTexture tex -- note, this is the font not the drawing surface
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
  useFBO fbo
  glViewport 0 0 magicW magicH
  action (putGlyph legend (F2 magicW magicH))
  glViewport 0 0 800 600
  useFBO (FBO 0)
  assertGL "yoooo"

putGlyph :: ULegend2 -> Float2 -> Glyph -> Float3 -> Float3 -> Int -> Int -> IO ()
putGlyph ul (F2 surfW surfH) (Glyph c) fg bg i j = do
  let F3 r1 g1 b1 = fg
  let F3 r2 g2 b2 = bg
  let Rect sx sy sw sh = glyphSrc c
  let Rect dx dy dw dh = glyphDst i j
  setUniform2f (ul2SurfWH ul) surfW surfH
  setUniform2f (ul2SrcXY ul) sx sy
  setUniform2f (ul2SrcWH ul) sw sh
  setUniform2f (ul2DstXY ul) dx dy
  setUniform2f (ul2DstWH ul) dw dh
  setUniform3f (ul2FG ul) r1 g1 b1
  setUniform3f (ul2BG ul) r2 g2 b2
  renderQuad

slap :: VBO -> VAO -> Shader -> ULegend1 -> Rect Float -> Tex -> IO ()
slap vbo vao shader ul (Rect x y w h) tex = do
  useVAO vao
  useVBO vbo
  useShader shader
  useTexture tex
  setUniform2f (ul1WinWH ul) 800 600
  setUniform2f (ul1SrcXY ul) 0 0
  setUniform2f (ul1SrcWH ul) w h
  setUniform2f (ul1DstXY ul) x y
  setUniform2f (ul1DstWH ul) w h
  renderQuad




magicW :: Num a => a
magicW = 400

magicH :: Num a => a
magicH = 600
