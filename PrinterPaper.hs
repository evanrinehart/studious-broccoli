{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module PrinterPaper where

-- the printer paper is a graphical surface where basic text can be printed
-- when fed commands, the surface will be updated. It can be used to display
-- useful data or show a basic UI. I'm focusing on data tables right now. Columns.

-- the pp is presented as a "server". Internally it holds graphical resources and
-- possibly state variables. The rest of the program accesses it through an IO API.

import Control.Monad
import Data.Char

import Common
import Glue
  (Tex,FBO(..),renderQuad, useVAO,useVBO, useShader, useTexture
  ,clearColorBuffer, useFBO, assertGL)
import Forge (Graphics(..),Ruler,gfxToTextTool)
import Paint (Paint(..),configUniforms)
import HList

data PrinterPaper = PrinterPaper
  { burnMany :: Float3 -> Float3 -> Int2 -> String -> IO ()
  , burnCmd  :: Int -> String -> IO ()
  , clear   :: IO () 
  , present :: IO () }

paperW = 114
paperH = 46
blank = Glyph 94

_burnMany :: Graphics -> Float3 -> Float3 -> Int2 -> String -> IO ()
_burnMany gfx fg bg (I2 x y) chars = do
  let withPaper = gfxToTextTool gfx
  withPaper $ \burn -> do
    forM_ (zip [0..] chars) $ \(i,c) -> do
      if y < paperH-1
        then burn (Glyph (ord c - 33)) fg bg (x+i) y
        else return () -- don't clobber cmd line area

_burnCmd :: Graphics -> Int -> String -> IO ()
_burnCmd gfx cur chars = do
  let withPaper = gfxToTextTool gfx
  withPaper $ \burn -> do
    forM_ [0..paperW-1] (\i -> burn blank white black i (paperH - 1))
    forM_ (zip [0..] chars) $ \(i,c) -> do
      if i==cur
        then burn (Glyph (ord c - 33)) black white i (paperH - 1)
        else burn (Glyph (ord c - 33)) white black i (paperH - 1)

_present :: Ruler -> Graphics -> IO ()
_present ruler gfx@Graphics{gfxSurf1=surf} = do
  F2 winW winH <- ruler
  renderTexturedRectangle ruler gfx (Rect 0 0 winW winH) surf

_clear :: Graphics -> IO ()
_clear Graphics{gfxFBO1=fbo, gfxSurf1=surf} = do
  useFBO fbo
  clearColorBuffer 1 1 1
  useFBO (FBO 0)
  assertGL "priter _clear caused an error"



-- bundle the magic tools to create a magic paper to write on
package :: Ruler -> Graphics -> PrinterPaper
package ruler gfx = PrinterPaper
  { burnMany = _burnMany gfx
  , burnCmd  = _burnCmd gfx
  , clear    = _clear gfx
  , present  = _present ruler gfx }


renderTexturedRectangle :: Ruler -> Graphics -> Rect Float -> Tex -> IO ()
renderTexturedRectangle ruler Graphics{gfxTile=vbo,gfxPaint3=Paint vao shader uniforms} (Rect x y w h) tex = do
  F2 winW winH <- ruler
  useVAO vao
  useVBO vbo
  useShader shader
  useTexture tex
  configUniforms uniforms $
    Field @"winWH" (F2 winW winH) >:
    Field @"srcXY" (F2 0 0) >:
    Field @"srcWH" (F2 w h) >:
    Field @"dstXY" (F2 x y) >:
    Field @"dstWH" (F2 w h) >:
    R0
  renderQuad
