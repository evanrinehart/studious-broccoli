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

import MiniApp
import TheThing

main :: IO ()
main = do

  GLFW.setErrorCallback $ Just $ \err msg -> do
    print err
    putStrLn msg
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

  cullBackFaces

  -- GLFW callbacks
  events <- newIORef []

  let push :: Event -> IO ()
      push e = modifyIORef' events (e:)

  let r2f = realToFrac
  GLFW.setWindowSizeCallback win $ Just $ \win w h -> push (WinSize w h)
  GLFW.setKeyCallback win $ Just $ \win key n state mods -> push (Keyboard key state n)
  GLFW.setCharCallback win $ Just $ \win c -> push (Typing c)
  GLFW.setMouseButtonCallback win $ Just $ \win button state mods -> push (MouseClick button state)
  GLFW.setCursorPosCallback win $ Just $ \win x y -> push (MouseMotion (r2f x) (r2f y))
  GLFW.setScrollCallback win $ Just $ \win dx dy -> push (Scroll (r2f dx) (r2f dy))

  assertGL "preliminaries"

  -- Fun ahead
  --let console = Console.empty (114 `div` 2) 46
  kit <- forgeAppKit win events
  mainLoop win kit
  GLFW.terminate

forgeAppKit :: GLFW.Window -> IORef [Event] -> IO AppKit
forgeAppKit win events = do

  let getWindowDims = GLFW.getWindowSize win >>= \(w,h) -> return $ F2 (fi w) (fi h)

  vbo <- loadBasicTile
  basic3 <- loadBasicShader vbo
  sheet <- loadTextureFromFile "dungeon.png"
  let useDungeon = newSpriteTool getWindowDims sheet vbo basic3

  font3 <- loadGlyphShader vbo
  font <- loadTextureFromFile "cozette.png"
  (fbo,surf) <- newTextSurface
  let textTool = newTextTool vbo font font3 fbo
  
  (v1,s1,l1) <- loadStraightShader vbo
  
  let awYeah dst tex = slap vbo v1 s1 l1 dst tex 

  let offwhite  = (F3 0.9 0.9 0.9)
  let black     = (F3 0 0 0)

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

  Just t <- GLFW.getTime
  prev <- newIORef t

  getTick <- newTicker
  oldTick <- newIORef 0

  pc1 <- newPerformanceComputer pc1View
  pc2 <- newPerformanceComputer pc2View

  cc <- newIORef 0

  thing <- makeTheThing textTool

  return $ AppKit
    { appWin = win
    , appQuit = GLFW.setWindowShouldClose win True
    , appTakeEvents = atomicModifyIORef events (\es -> ([], es))
    , appDungeon = useDungeon
    , appText = textTool
    , appSlap = awYeah
    , appVarPrev = prev
    , appGetTick = getTick
    , appOldTick = oldTick
    , appPC1 = pc1
    , appPC2 = pc2
    , appThing = thing
    , appMinis = [(Rect 0 0 400 600, surf, thing)]
    , appVarCC = cc }

data AppKit = AppKit
  { appWin         :: GLFW.Window
  , appQuit        :: IO ()
  , appTakeEvents  :: IO [Event]
  , appDungeon     :: SpriteTool
  , appText        :: TextTool
  , appSlap        :: Rect Float -> Tex -> IO ()
  , appVarPrev     :: IORef Double
  , appGetTick     :: IO Int
  , appOldTick     :: IORef Int
  , appPC1         :: Double -> IO Double
  , appPC2         :: Double -> IO Double
  , appVarCC       :: IORef Int
  , appThing       :: MiniApp
  , appMinis       :: [(Rect Float, Tex, MiniApp)] }


-- for the interframe delta where less than 33ms is interpreted as a hit
pc1View :: [Double] -> Double
pc1View xs = 100 * hits / (hits + misses) where
  hits :: Double
  hits = foldl' (+) 0 (map (\x -> if x/33 < 1 then 1 else 0) xs)
  misses :: Double
  misses = foldl' (+) 0 (map (\x -> ffloor (x/33)) xs)

-- for health of the main loop body which ought to take at most 16.666ms
-- formula: divide 16.666 by the measured time, log10, avg, exp10
pc2View :: [Double] -> Double
pc2View = (\x -> 10000 - x) . (*10000) . avg . map (\x -> x / 16.666) where
  avg xs = sum xs / fi (length xs)

ffloor :: Double -> Double
ffloor x = fi (floor x :: Int)

-- time notes
-- need frame delta for 'bogofps' percent (need Q, need prev time)
-- need update duration for frame score (100 * (16.666 / dur)) (need Q)
-- need game step counter for fixed time update speed

mainLoop :: GLFW.Window -> AppKit -> IO ()
mainLoop win kit = do

  let var = appOldTick kit
  that <- readIORef var
  this <- appGetTick kit
  let n = this - that
  writeIORef var this

  -- { begin timed section
  ((), intraFrameTime) <- stopwatch $ do

    -- pump the event handlers (and windowing system)
    GLFW.pollEvents
    es <- appTakeEvents kit

    forM_ [(m,e)|e<-es,m<-appMinis kit] $ \((_, _, mini),e) -> maPoke mini e

    forM_ es $ \e -> case e of
      Keyboard GLFW.Key'Escape _ _ -> appQuit kit
      _ -> return ()


    -- time passes for animating subprocess
    forM_ (appMinis kit) $ \(_, _, mini) -> maTime mini n


    -- render orbitting sprites and the console
    Just t <- fmap (fmap realToFrac) GLFW.getTime
    let d = 2 * pi / 5

    forM_ (appMinis kit) $ \(_, _, mini) -> maShow mini

    clearColorBuffer

    -- some sprites
    appDungeon kit $ \brush -> do
      let f s phi = let x=64*sin phi + 600; y=64*cos phi+300 in brush (from s) (to x y)
      f bricks t
      f orb (t + d)
      f monster (t + 2*d)
      f ogre (t + 3*d)
      f knight (t + 4*d)

    -- display all miniapp panes
    forM_ (appMinis kit) $ \(dst, tex, _) -> appSlap kit dst tex
  -- end timed section }

  -- performance computations
  prev <- readIORef (appVarPrev kit)
  Just now <- GLFW.getTime
  let interFrameDelta = 1000 * (now - prev)
  writeIORef (appVarPrev kit) now
  percent <- appPC1 kit interFrameDelta
  rt <- appPC2 kit intraFrameTime

  -- temporary, print out performance every 30 frames
  let ref = appVarCC kit
  i <- readIORef ref
  when (i `mod` 60 == 0) $ do
    putStr (printf "%05.1f" percent ++ "%")
    putStrLn (" (" ++ printf "%04d" (max 0 (floor rt :: Int)) ++ ")")
  writeIORef ref (i+1)

  -- show graphics / sleep
  GLFW.swapBuffers win
  GLFW.windowShouldClose win >>= \b -> case b of
    False -> mainLoop win kit
    True  -> return ()




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
