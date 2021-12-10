{-# LANGUAGE BlockArguments #-}
module Forge where

import Data.IORef
import Data.Foldable

import qualified Graphics.UI.GLFW as GLFW

import Common
import TextMode
import SpriteMode
import Event
import Paint
import Glue
import Ticks

import Performance


type Ruler = IO Float2


-- common graphics resources (tile, sheets, font, paint)
-- surface though isn't common enough
data Graphics = Graphics
  { gfxTile :: VBO
  , gfxDung :: Tex
  , gfxFont :: Tex
  , gfxPaint1 :: Paint UL1
  , gfxPaint2 :: Paint UL2
  , gfxPaint3 :: Paint UL1 -- straight paint
  , gfxSurf1 :: Tex -- move out
  , gfxFBO1 :: FBO  -- move out
  }

loadGfx :: IO Graphics
loadGfx = do
  vbo <- loadBasicTile
  paint3 <- loadStraightShader vbo
  (fbo,surf) <- newTextSurface
  Graphics <$>
    pure vbo <*>
    loadTextureFromFile "dungeon.png" <*>
    loadTextureFromFile "cozette.png" <*>
    loadBasicShader vbo <*>
    loadGlyphShader vbo <*>
    pure paint3 <*>
    pure surf <*>
    pure fbo



gfxToTextTool :: Graphics -> TextTool
gfxToTextTool Graphics{gfxTile=tile,gfxFont=font,gfxPaint2=paint,gfxFBO1=fbo} =
  newTextTool tile font paint fbo

forgeTools :: Ruler -> Graphics -> IO (SpriteTool, TextTool, Slapper)
forgeTools ruler Graphics{gfxTile=vbo,gfxDung=dung,gfxFont=font,gfxPaint1=paint1,gfxPaint2=paint2,gfxPaint3=paint3,gfxSurf1=surf,gfxFBO1=fbo} = do
  
  let spriteTool = newSpriteTool ruler dung vbo paint1
  let textTool = newTextTool vbo font paint2 fbo
  let slapper dst tex = slap vbo paint3 dst tex 

  textTool \burn -> do
    burn (Glyph 32) (F3 0.9 0.9 0.9) (F3 0 0 0) 0 0
    burn (Glyph 33) (F3 0.9 0.9 0.9) (F3 0 0 0) 1 0
    burn (Glyph 34) (F3 0.9 0.9 0.9) (F3 0 0 0) 2 0
    burn (Glyph 35) (F3 0.9 0.9 0.9) (F3 0 0 0) 3 0
    burn (Glyph 36) (F3 0.9 0.9 0.9) (F3 0 0 0) 0 1
    burn (Glyph 37) (F3 0.9 0.9 0.9) (F3 0 0 0) 1 1
    burn (Glyph 38) (F3 0.9 0.9 0.9) (F3 0 0 0) 2 1
    burn (Glyph 39) (F3 0.9 0.9 0.9) (F3 0 0 0) 3 1
    burn (Glyph 40) (F3 0.9 0.9 0.9) (F3 0 0 0) 0 2
    burn (Glyph 41) (F3 0.9 0.9 0.9) (F3 0 0 0) 1 2

  return (spriteTool, textTool, slapper)


buyPerformanceStuff :: IO PerformanceStuff
buyPerformanceStuff = do
  let uhg = GLFW.getTime >>= \mx -> case mx of
              Nothing -> error "GLFW.getTime returned Nothing"
              Just x  -> return x
  newPerformanceStuff uhg
  
makeWindowRuler win = do
  (w,h) <- GLFW.getWindowSize win
  return $ F2 (fi w) (fi h)

{-
forgeAppKit :: GLFW.Window -> IORef [Event] -> IO AppKit
forgeAppKit win events = do

  gfx@Graphics{gfxTile=vbo,gfxDung=dung,gfxFont=font,gfxPaint1=paint1,gfxPaint2=paint2,gfxPaint3=paint3,gfxSurf1=surf,gfxFBO1=fbo} <- loadGfx

  (spriteTool,textTool,slapper) <- forgeTools gfx

  cc <- newIORef 0

  --let cg = ConsoleGobbler{myTool=textTool,mySlapper=slapper,myTile=fbo,mySurf=surf,myPaint1=paint1,myPaint2=paint2}

  return $ AppKit
    { appWin = win
    , appQuit = GLFW.setWindowShouldClose win True
    , appTakeEvents = atomicModifyIORef events (\es -> ([], es))
    , appPerfBracket = performanceBracket stuff
    , appGetMetrics = readIORef (rMetrics stuff)
    , appText = textTool
    , appSlap = slapper
    , appVarCC = cc }
-}



