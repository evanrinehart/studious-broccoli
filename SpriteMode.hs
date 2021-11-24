module SpriteMode where

import qualified Data.Vector.Storable as V
import Data.Vector.Storable (Vector)
import System.Exit

import Graphics.GL
import Codec.Picture

import Common
import Glue

type BurnSprite = Rect Float -> Rect Float -> IO ()
type SpriteTool = (BurnSprite -> IO ()) -> IO ()

-- things that close over:
-- shader + uniform locations
-- vao + attribs
-- texture
-- vbo

{-
makeSpriteLoader :: Measure -> IO (FilePath -> IO SpriteTool)
makeSpriteLoader measure = do
  vao <- newVAO
  useVAO vao

  let name1 = "basic.vs"
  let name2 = "basic.fs"
  code1 <- readFile name1
  code2 <- readFile name2
  shader <- newShader name1 code1 name2 code2
  vbo <- storableVectorToVBO tileData
  useVBO vbo
  configAttrib shader "position" 2 16 0 GL_FLOAT
  configAttrib shader "texcoord" 2 16 8 GL_FLOAT
  ul0 <- getUniformLocation shader "winWH"
  ul1 <- getUniformLocation shader "srcXY"
  ul2 <- getUniformLocation shader "srcWH"
  ul3 <- getUniformLocation shader "dstXY"
  ul4 <- getUniformLocation shader "dstWH"

  assertGL "sprite loader maker"

  let legend = UL1 ul0 ul1 ul2 ul3 ul4

  return (loadSheet measure vao vbo shader legend)

-- generate and close over texture
loadSheet :: Measure -> VAO -> VBO -> Shader -> ULegend1 -> FilePath -> IO SpriteTool
loadSheet measure vao tile shader legend filename = do
  tex <- readImage filename >>= \x -> case x of
    Left msg -> putStrLn (filename ++ ": " ++ msg) >> exitFailure
    Right di -> pictureToTex di
  assertGL "load image file"
  useTexture tex
  return $ \action -> do
    winDims <- measure
    useVAO vao
    useVBO tile
    useShader shader
    useTexture tex
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
    action (putSprite winDims legend)
-}

loadBasicTile :: IO VBO
loadBasicTile = storableVectorToVBO tileData

loadTextureFromFile :: FilePath -> IO Tex
loadTextureFromFile path = do
  tex <- readImage path >>= \x -> case x of
    Left msg -> putStrLn (path ++ ": " ++ msg) >> exitFailure
    Right di -> pictureToTex di
  assertGL "load image file"
  return tex

loadBasicShader :: VBO -> IO (Shader,VAO,ULegend1)
loadBasicShader vbo = do
  vao <- newVAO
  useVAO vao
  let name1 = "basic.vs"
  let name2 = "basic.fs"
  code1 <- readFile name1
  code2 <- readFile name2
  shader <- newShader name1 code1 name2 code2
  useVBO vbo
  configAttrib shader "position" 2 16 0 GL_FLOAT
  configAttrib shader "texcoord" 2 16 8 GL_FLOAT
  ul0 <- getUniformLocation shader "winWH"
  ul1 <- getUniformLocation shader "srcXY"
  ul2 <- getUniformLocation shader "srcWH"
  ul3 <- getUniformLocation shader "dstXY"
  ul4 <- getUniformLocation shader "dstWH"

  return (shader, vao, UL1 ul0 ul1 ul2 ul3 ul4)

newSpriteTool :: IO Float2 -> Tex -> VBO -> (Shader,VAO,ULegend1) -> SpriteTool
newSpriteTool measure tex vbo (shader,vao,legend) action = do
  winDims <- measure
  useVAO vao
  useVBO vbo
  useShader shader
  useTexture tex
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
  action (putSprite winDims legend)

putSprite :: Float2 -> ULegend1 -> Rect Float -> Rect Float -> IO ()
putSprite (F2 winw winh) ul (Rect sx sy sw sh) (Rect dx dy dw dh) = do
  setUniform2f (ul1WinWH ul) winw winh
  setUniform2f (ul1SrcXY ul) sx sy
  setUniform2f (ul1SrcWH ul) sw sh
  setUniform2f (ul1DstXY ul) dx dy
  setUniform2f (ul1DstWH ul) dw dh
  renderQuad
