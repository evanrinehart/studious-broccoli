{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
module Main where

import Data.IORef
import Control.Monad
import Data.Foldable
import Data.Char

import qualified Graphics.UI.GLFW as GLFW
--import Graphics.GL

import Common
import Glue
import Event
import Ticks
import Rainbow

import CmdLine


data MiniApp = MA
  { maHandle :: [Event] -> IO ()
  , maUpdate :: IO ()
  , maRender :: IO () }



makeDebugger gfx = do

  canvas <- newCanvas 320 480
  useFBO (canvasFbo canvas)
  clearColorBuffer 1 1 1
  useFBO (FBO 0)

  let white = F3 1 1 1
  let black = F3 0 0 0

  --let box = 33 + 40 * 35 + 30 + 369
  --darkbox = 33 + 40 * 35 + 29

  var <- newIORef (0,"")
  let actions = cache var cmdLine

  let act1 = do
        (cur,buf) <- readIORef var
        useFBO (canvasFbo canvas)
        clearColorBuffer 1 1 1
        withFont gfx canvas $ \torch -> do
          forM_ (zip [0..] (buf ++ " ")) $ \(i,c) -> do
            if cur==i
              then torch c white black (I2 i 36)
              else torch c black white (I2 i 36)

  return $ Widget{
    widgetRepaint=act1,
    widgetCanvas=canvas,
    widgetActions=actions
  }

makeArena :: GFX -> IO Widget
makeArena gfx = do
  canvas <- newCanvas 320 480
  (sheet,_) <- loadTextureFromFile "dungeon.png"
  useFBO (canvasFbo canvas)
  clearColorBuffer 0 0 0
  useFBO (FBO 0)

  let act1 = do
        withSprites gfx sheet canvas $ \torch -> do
          forM_ [(x,y)|x<-[0..12],y<-[0..12]] $ \(i,j) -> do
            torch (j*13 + i) (F2 (fi (i*32 - 640`div`4)) (fi (j*32 - 480`div`2 + 64)))
  return $ Widget{
    widgetRepaint=act1,
    widgetCanvas=canvas,
    widgetActions=pure(pure())
  }


data MainLoop = MainLoop
  { mlClock :: IO Int
  , mlDoEvents :: IO ()
  , mlDead :: IO Bool
  , mlRender :: IO ()
  , mlDoTick :: IO ()
  }

mainLoop :: MainLoop -> IO ()
mainLoop md = mlClock md >>= loop where
  loop prev = do
    mlDoEvents md

    now <- mlClock md
    let delta = now - prev
    replicateM_ delta (mlDoTick md)

    mlRender md

    dead1 <- mlDead md
    let dead2 = False
    when (not (dead1 || dead2)) (loop now)

-- map a pure interactive object to the IO world using an MVar
-- also, update it's display?
cache :: IORef a -> UserActions (a -> (a,b)) -> UserActions (IO ())
cache var ua = fmap f ua where
  f g = do
    msg <- atomicModifyIORef' var g
    return ()

main = do
  -- let there be a window plus graphics
  (win,gfx) <- rainbow 640 480 1 -- create window load graphics

  debugger <- makeDebugger gfx
  arena    <- makeArena gfx

  -- let there be a connection between the user and... something
  jackIn win (quitter win $ widgetActions debugger) -- interaction with command line

  -- a clock used by the main loop
  ticker <- newTicker

  let splat1 = blitCanvas gfx (widgetCanvas arena) (F2 0 (-240))
  let splat2 = blitCanvas gfx (widgetCanvas debugger) (F2 (-320) (-240))

  mainLoop $ MainLoop{
    mlClock=ticker,
    mlDoEvents=GLFW.pollEvents,
    mlDead=GLFW.windowShouldClose win,
    mlRender=do
      clearColorBuffer 0 1 0
      widgetRepaint debugger
      splat1
      splat2
      GLFW.swapBuffers win,
    mlDoTick=return ()
  }


    --blitCanvas gfx canvas (F2 0 (-240))

-- a world is a common environment for all components to inhabit.
-- why is it necessary? Because, we only want to render objects that
-- are visible. So there needs to be a concept of visibility. An
-- object has a visible area and a depth. If it's covered by other
-- objects it's invisible. Also it may be hidden on purpose, and so
-- automatically invisible regardless of area.

data Widget = Widget
  { --wdgArea :: Float4
--  , depth :: Int -- auto computed from stacking order
--  , hidden :: Bool
--  , myID :: Int
    widgetRepaint :: IO (),
    widgetCanvas  :: Canvas,
    widgetActions :: UserActions (IO ())
  }



quitter :: GLFW.Window -> UserActions (IO ()) -> UserActions (IO ())
quitter win orig = orig{uaKeydown = g} where
  g key mods = do
    case key of
      KBEscape -> GLFW.setWindowShouldClose win True
      _        -> return ()
    uaKeydown orig key mods
