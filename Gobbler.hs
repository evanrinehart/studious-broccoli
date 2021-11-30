-- the gobbler!!! haha!
{-# LANGUAGE NamedFieldPuns #-}
module Gobbler where

import Data.Maybe
import Data.Char
import Data.IORef
import Control.Monad
import Control.Exception

import Glue
import Paint
import Common
import TextMode
import Forge
import U

import Console

data ConsoleGobbler = CG
  { myE1     :: IORef (E (Int,String))
  , myE2     :: IORef (E String)
  , myV1     :: IORef (V (Int,String))
  , myTool   :: TextTool
  , mySlapper :: Slapper
  , myTile   :: VBO
  , mySurf   :: Tex
  , myFont   :: Tex
  , myPaint1 :: Paint UL1
  , myPaint2 :: Paint UL2 }

-- two input events
-- 1. the current state of the buffer is delivered, suggest you re-render the picture
-- 2. command line entered, suggest running the command someho
hatchGobbler :: Graphics -> (a,TextTool,Slapper) -> (E (Int,String), E String) -> IO (IO ())
hatchGobbler
  Graphics{gfxTile=vbo,gfxPaint1=paint1,gfxPaint2=paint2,gfxFont=font,gfxSurf1=surf}
  (_,tt,slapper)
  (e1,e2) = do
    ref1 <- newIORef e1
    ref2 <- newIORef e2
    ref3 <- newIORef undefined
    (return . gobbler) $ CG ref1 ref2 ref3 tt slapper vbo surf font paint1 paint2

gobbler :: ConsoleGobbler -> IO ()
gobbler (CG{myE1,myE2,myV1,myTool,mySlapper,myTile,mySurf,myFont,myPaint1,myPaint2}) = do
  e1 <- atomicModifyIORef' myE1 (\(E (e:ee)) -> (E ee, e))
  e2 <- atomicModifyIORef' myE2 (\(E (e:ee)) -> (E ee, e))
  --(i,buf) <- atomicModifyIORef' myV1 (\(V ((v,_):vv)) -> (V vv, v))

  forM_ e2 $ \cmd -> do
    putStrLn cmd

  case e1 of
    Nothing -> return ()
    Just (cur,buf) -> do

      myTool $ \burn -> do
        forM_ [0..114] $ \x -> burn (Glyph 94) white black x 45
        forM_ (zip [0..114] (buf++" ")) $ \(i,c) -> do
          if (i == cur)
            then burn (Glyph (ord c - 33)) black white i 45
            else burn (Glyph (ord c - 33)) white black i 45

  -- display the surface
  mySlapper (Rect 0 0 800 600) mySurf

helper myTool cur buf = do
  myTool $ \burn -> do
    forM_ [0..114] $ \x -> burn (Glyph 94) white black x 45
    forM_ (zip [0..114] (buf++" ")) $ \(i,c) -> do
      if (i == cur)
        then burn (Glyph (ord c - 33)) black white i 45
        else burn (Glyph (ord c - 33)) white black i 45
