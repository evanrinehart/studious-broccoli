{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Data.IORef
import Control.Monad
import Data.Foldable
--import System.Random
--import Numeric
--import Text.Printf
import Data.Char

import qualified Graphics.UI.GLFW as GLFW
--import qualified Data.IntMap as IM

--import Graphics.GL

import Common
import Glue
import Event
import Ticks
import Rainbow

data MainDriver = MainDriver
  { mdClock :: IO Int
  , mdEvents :: IO [Event]
  , mdDead :: IO Bool
  , mdRender :: IO ()
  , mdDoTick :: IO ()
  , mdHandler :: [Event] -> IO ()
  }

mainLoop :: MainDriver -> IO ()
mainLoop md = mdClock md >>= loop where
  loop prev = do
    evs <- mdEvents md
    when (not (null evs)) (mdHandler md evs)

    now <- mdClock md
    let delta = now - prev
    replicateM_ delta (mdDoTick md)

    mdRender md

    dead1 <- mdDead md
    let dead2 = any isEscape evs
    when (not (dead1 || dead2)) (loop now)



data MiniApp = MA
  { maHandle :: [Event] -> IO ()
  , maUpdate :: IO ()
  , maRender :: IO () }

debugger gfx = do

  canvas <- newCanvas 320 480
  useFBO (canvasFbo canvas)
  clearColorBuffer 1 1 1
  useFBO (FBO 0)

  let white = F3 1 1 1
  let black = F3 0 0 0

  --let box = 33 + 40 * 35 + 30 + 369
  --darkbox = 33 + 40 * 35 + 29

  return $ MA{
    maHandle=(\es -> do
      print es
      withFont gfx canvas $ \torch -> do
        forM_ [(x,y)|x<-[0..44],y<-[0..35]] $ \(i,j) -> do
          torch 'A' black white (I2 i j)
    ),
    maUpdate=return (),
    maRender=blitCanvas gfx canvas (F2 (-320) (-240))
  }

arena gfx = do
  canvas <- newCanvas 320 480
  (sheet,_) <- loadTextureFromFile "dungeon.png"
  useFBO (canvasFbo canvas)
  clearColorBuffer 0 0 0
  useFBO (FBO 0)

  return $ MA{
    maHandle=(\es -> do
      withSprites gfx sheet canvas $ \torch -> do
        forM_ [(x,y)|x<-[0..12],y<-[0..12]] $ \(i,j) -> do
          torch (j*13 + i) (F2 (fi (i*32 - 640`div`4)) (fi (j*32 - 480`div`2 + 64)))
    ),
    maUpdate=return (),
    maRender=blitCanvas gfx canvas (F2 0 (-240))
  }

main = do
  var <- newIORef []
  (win,gfx) <- rainbow 640 480 2 var

  ticker <- newTicker

  app1 <- debugger gfx
  app2 <- arena gfx

  mainLoop $ MainDriver{
    mdClock=ticker,
    mdEvents=GLFW.pollEvents >> atomicModifyIORef' var (\x -> ([], x)),
    mdDead=GLFW.windowShouldClose win,
    mdRender=clearColorBuffer 0 1 0 >> maRender app1 >> maRender app2 >> GLFW.swapBuffers win,
    mdDoTick=maUpdate app1 >> maUpdate app2,
    mdHandler=(\e -> maHandle app1 e >> maHandle app2 e)
  }

