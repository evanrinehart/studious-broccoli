{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import System.Exit
import Data.IORef
import Control.Monad (when, forM, forM_)
import Data.Foldable
import System.Random
import Numeric
import Text.Printf
import Data.Char

import qualified Graphics.UI.GLFW as GLFW
import qualified Data.IntMap as IM

import Graphics.GL

import Common
import Glue
import TextMode
import SpriteMode
import Console
--import DebugPanel
import Event
import Ticks
import Paint

import Forge
import U

import Performance

import Gobbler

glfwRitual = do
  GLFW.setErrorCallback $ Just $ \err msg -> do
    putStrLn ("Error: " ++ show err ++ " ; " ++ msg)
    GLFW.terminate
    exitFailure

  -- GLFW init graphics
  GLFW.init -- returns False on failure

  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 2)
  GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)

  -- summon a window
  Just win <- GLFW.createWindow 800 600 ":D" Nothing Nothing 
  GLFW.makeContextCurrent (Just win)

  return win


glfwCallbacks win = do
  events <- newIORef []

  let push :: Event -> IO ()
      push e = do
        modifyIORef' events (e:)

  let r2f = realToFrac
  GLFW.setWindowSizeCallback win $ Just $ \win w h -> push (WinSize w h)
  GLFW.setKeyCallback win $ Just $ \win key n state mods -> push (Keyboard key state mods n)
  GLFW.setCharCallback win $ Just $ \win c -> push (Typing c)
  GLFW.setMouseButtonCallback win $ Just $ \win button state mods -> push (MouseClick button state)
  GLFW.setCursorPosCallback win $ Just $ \win x y -> push (MouseMotion (r2f x) (r2f y))
  GLFW.setScrollCallback win $ Just $ \win dx dy -> push (Scroll (r2f dx) (r2f dy))

  return events


miscOtherStuff = do
  cullBackFaces -- debug only



{- *** Main Loop *** -}

mainLoop win kit = do

  -- { begin timed section
  performanceBracket (appGauge kit) $ do
    -- populate appEvents
    GLFW.pollEvents
    
    clearColorBuffer 0 1 0
    readIORef (appGobblers kit) >>= sequence_ -- deploy the gobblers

    writeIORef (appEvents kit) []
  -- end timed section }

  -- temporary, print out performance every 30 frames
  let cc = appCC kit
  i <- readIORef cc
  when (i `mod` 60 == 0) $ do
    (health,armor) <- (readIORef . rMetrics . appGauge) kit
    putStr (printf "%05.1f" health ++ "%")
    putStrLn (" (" ++ printf "%04d" (max 0 armor) ++ ")")
  writeIORef cc (i+1)

  -- show graphics / sleep
  GLFW.swapBuffers win
  GLFW.windowShouldClose win >>= \b -> case b of
    False -> mainLoop win kit
    True  -> return ()

{- *** End Main Loop *** -}




-- misc
bricks  = (8,3)
orb     = (1,8)
monster = (11,3)
ogre    = (5,11)
knight  = (5,7)

to :: Float -> Float -> Rect Float
to x y = Rect dx dy w w where
  dx = x - w/2
  dy = y - w/2
  w = 32

from :: (Int,Int) -> Rect Float
from (i,j) = (Rect sx sy 32 32) where
  sx = fi i * 32
  sy = fi j * 32

basicMiniApp :: MiniApp
basicMiniApp = this where
  f e = print e
  g n = putStrLn ("time + " ++ show n)
  h = putStrLn "repaint"
  this = MiniApp
    { maPoke = f
    , maTime = g
    , maShow = h }

-- ... ... ...



hireOracle :: IORef [Event] -> E [Event]
hireOracle ref = sysE $ do
  es <- readIORef ref
  case es of
    [] -> return Nothing
    _  -> do
      return (Just es)

data AppKit = AppKit
  { appEvents :: IORef [Event]
  , appGauge :: PerformanceStuff
  , appGobblers :: IORef [IO ()]
  , appCC :: IORef Int
  }

main = do

  win    <- glfwRitual 
  events <- glfwCallbacks win

  gfx    <- loadGfx
  let ruler = makeWindowRuler win
  tools  <- forgeTools ruler gfx
  gauge  <- buyPerformanceStuff

  let src = hireOracle events

  let artificialBS = E [Just [Keyboard GLFW.Key'Backspace GLFW.KeyState'Pressed undefined 0]]
  let thing = cmdLine 256 (src <> artificialBS)

  gobble <- hatchGobbler gfx tools thing
  gobblers <- newIORef [gobble]

  counter <- newIORef 0

  let kit = AppKit events gauge gobblers counter

  mainLoop win kit
