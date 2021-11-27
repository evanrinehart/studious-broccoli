{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module SpriteMode where

import qualified Data.Vector.Storable as V
import Data.Vector.Storable (Vector)
import System.Exit

import Graphics.GL
import Codec.Picture

import Common
import Glue
import HList
import Paint

type BurnSprite = Rect Float -> Rect Float -> IO ()
type SpriteTool = (BurnSprite -> IO ()) -> IO ()

-- things that close over:
-- shader + uniform locations
-- vao + attribs
-- texture
-- vbo

loadBasicTile :: IO VBO
loadBasicTile = storableVectorToVBO tileData

loadTextureFromFile :: FilePath -> IO Tex
loadTextureFromFile path = do
  tex <- readImage path >>= \x -> case x of
    Left msg -> putStrLn (path ++ ": " ++ msg) >> exitFailure
    Right di -> pictureToTex di
  assertGL "load image file"
  return tex

loadBasicShader :: VBO -> IO (Paint UL1)
loadBasicShader vbo = do
  vao <- newVAO
  useVAO vao
  let name1 = "basic.vs"
  let name2 = "sprite.fs"
  code1 <- readFile name1
  code2 <- readFile name2
  shader <- newShader name1 code1 name2 code2
  useVBO vbo
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

newSpriteTool :: IO Float2 -> Tex -> VBO -> Paint UL1 -> SpriteTool
newSpriteTool measure tex vbo (Paint vao shader uniforms) action = do
  winDims <- measure
  useVAO vao
  useVBO vbo
  useShader shader
  useTexture tex
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
  action (putSprite winDims uniforms)

putSprite :: Float2 -> Gate UL1 -> Rect Float -> Rect Float -> IO ()
putSprite winWH uniforms (Rect sx sy sw sh) (Rect dx dy dw dh) = do
  configUniforms uniforms $
    Field @"winWH" winWH >:
    Field @"srcXY" (F2 sx sy) >:
    Field @"srcWH" (F2 sw sh) >:
    Field @"dstXY" (F2 dx dy) >:
    Field @"dstWH" (F2 dw dh) >:
    R0
  renderQuad
