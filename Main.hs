{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NegativeLiterals #-}
module Main where

import Data.IORef
import Control.Monad
import Control.Applicative
import Data.Foldable
import Data.Char
import Data.Maybe

import Control.Exception
import Control.Concurrent
import Control.Monad.Writer

import Control.Concurrent.STM
import qualified Data.Map as M; import Data.Map (Map)

import qualified Graphics.UI.GLFW as GLFW
--import Graphics.GL

import Codec.Picture as JuicyPixels

import Common
import Glue
import Event
import Ticks
import Rainbow
import Audio
import Widget

import Slider

import CmdLine

import CardGame
import Arkanoid
import Level
import Grid

import OneShot
import Shape


-- concentrate on getting widgets to work
-- repaint: a number of graphics resources are aquired and then needed for repainting later
-- user action: user actions must 1. map to the widget's actions 2. 


-- the arena test widget doesn't animate, doesn't cause any side effects
-- doesn't make any external requests
{-
makeArena :: GFX -> Float4 -> IO Widget
makeArena gfx area@(F4 x y w h) = do
  -- gfx resources
  canvas <- newCanvas (floor w) (floor h)
  (sheet,_) <- loadTextureFromFile "dungeon.png"

  -- initial state data
  let deck = map (\i -> let (x,y) = divMod i 13 in Card i (F4 (fi x * 32 - 160) (fi y * 32 - 240) 32 32) True) [0..168]

  -- evil mutable variables
  var1 <- newIORef deck
  var2 <- newIORef (F2 0 0)

  -- action wrapper maps "arena actions" to/from user actions properly
  let shift (F2 mx my) = F2 (mx - (x + w/2)) (my - (y + h/2))

  let actionWrapper = -- some of this is common to all widgets, foo is "custom"
        shiftMouse area .  -- shift mouse a fixed amount
        cacheMouse var2 .   -- cache mouse locations to ref, provide mouse location
        CardGame.foo .      -- remap useractions to cardgame actions, always require mouse location
        cachedState var1 -- IORef s + s->s = IO ()  (inner action API)
  let render gfx canvas sheet cards = do
        withSprites gfx sheet canvas $ \torch -> do
          forM_ (reverse cards) $ \(Card face (F4 x y w h) hidden) -> do
            torch (if hidden then 2 else face) (F2 x y)
        --withDisc gfx canvas $ \torch -> do
          --torch (F2 0 0) 5
  
  return $ Widget{
    widgetRepaint=readIORef var1 >>= render gfx canvas sheet,
    widgetCanvas=canvas,
    widgetArea=F4 x y w h,
    widgetActions=actionWrapper CardGame.basicGame,
    widgetFinalize=deleteCanvas canvas
  }
-}

{--
makeArkanoid gfx area@(F4 x y w h) mats = do
  canvas <- newCanvas (floor w) (floor h)

  var1 <- newArkVar

  --var1 <- newIORef (P (F2 159.99 80) (F2 0 (-1)))
  var2 <- newIORef (F2 0 0)

  --let pd = PD (F2 0 (-150)) (F2 50 10) 0

  -- action wrapper maps "arena actions" to/from user actions properly
  let shift (F2 mx my) = F2 (mx - (x + w/2)) (my - (y + h/2))

  let actionWrapper =
        shiftMouse area .
--        cacheMouse var2 .
--        (\ua -> ua { uaMouse = \x y -> print (x,y) >> uaMouse ua x y }) .
        cacheStateHandleAds var1 (\ads -> if null ads then return () else return ())--print ads)

  let actions :: (UserActions (IO ())) = actionWrapper (arkActions ap)


  let render gfx canvas (Ark lvl ball padx) = do
        useFBO (canvasFbo canvas)
        clearColorBuffer 0 0 0
        let F2 x y = particlePosition ball
        withBlock gfx canvas $ \torch -> do
          torch (F4 (-160) (-240) 1 480) (F3 1 0 0)
          torch (F4 (-160) (-240) 320 1) (F3 1 0 0)
          torch (F4 (-160) 239 320 1) (F3 1 0 0)
          torch (F4 159 (-240) 1 480) (F3 1 0 0)

          --print (estimateVelocity (padx : hist))
          let box = padDataToBox ap padx
          torch box (F3 0.5 0.5 0.5)

          let (AL _ _ blocks) = lvl
          forM_ (gridToList blocks) $ \((i,j), material) -> do
            let p0 = fi i * 32 - 320/2
            let p1 = 480/2 - fi (j+1)  * 32

            cs <- atomically (readTVar mats)
            torch (F4 p0 p1 32 32) (M.findWithDefault (F3 1 1 1) material cs)
{-
            torch (F4 (-160) 208 32 32) (F3 1 0 1)
            torch (F4 (-128) 208 32 32) (F3 1 1 0)
            torch (F4 (-96)  208 32 32) (F3 0 1 1)
            torch (F4 (-64)  208 32 32) (F3 0 1 0)
-}
        withDisc gfx canvas $ \torch -> do
          torch (F2 x y) (apBallRadius ap) (F3 0 1 0)
  
  return $ Widget{
    widgetRepaint=readIORef var1 >>= render gfx canvas,
    widgetCanvas=canvas,
    widgetArea=F4 x y w h,
    widgetActions=actions,
    widgetFinalize=deleteCanvas canvas
  }
--}


badMaterialColors :: Map Char Float3
badMaterialColors = M.fromList (zip "sgwcl" cs) where
  cs = [F3 0.3 0.3 0.3
       ,F3 0.7 0 0
       ,F3 0.4 0.2 0
       ,F3 0.7 0 0.7
       ,F3 0.8 0.8 0]

-- cache any mouse motion, then provide it to the wrapped actions
cacheMouse :: IORef Float2 -> UserActions (Float2 -> IO ()) -> UserActions (IO ())
cacheMouse ref actionf = action{uaMouse = g} where
  action = fmap h actionf
  h f = do
    xy <- readIORef ref
    f xy
  g x y = do
    writeIORef ref (F2 x y)
    uaMouse action x y


  
  


-- the debugger holds resources:
--   a canvas (texture, fbo)
--   an ioref with the current state

{--
makeDebugger gfx (F4 x y w h) = do
  canvas <- newCanvas (floor w) (floor h)
  var <- newIORef (0,"")
  return $ Widget{
    widgetCanvas=canvas,
    widgetFinalize=deleteCanvas canvas,
    widgetArea=F4 x y w h,
    widgetRepaint=do
      (cur,buf) <- readIORef var

      useFBO (canvasFbo canvas)
      clearColorBuffer 1 1 1
      useFBO (FBO 0)

      let white = F3 1 1 1
      let black = F3 0 0 0

      withFont gfx canvas $ \torch -> do
        forM_ (zip [0..] (buf ++ " ")) $ \(i,c) -> do
          if cur==i
            then torch c white black (I2 i 36)
            else torch c black white (I2 i 36),
    widgetActions=
      cacheStateHandleAds var (fromMaybe (return ()) . fmap print) cmdLine
  }
--}


-- how to make actions that find actions using io and use them

{-
dynamicActions :: UserActions (IO (UserActions (IO a))) -> UserActions (IO a)
dynamicActions uaua = UserActions
    { uaKeydown  = \x y -> uaKeydown uaua x y >>= \ua -> uaKeydown ua x y
    , uaKeyup    = \x y -> uaKeyup uaua x y   >>= \ua -> uaKeyup ua x y
    , uaKeyagain = \x y -> uaKeyagain uaua x y >>= \ua -> uaKeyagain ua x y
    , uaTyping   = \x   -> uaTyping uaua x    >>= \ua -> uaTyping ua x
    , uaMouse    = \x y -> uaMouse uaua x y   >>= \ua -> uaMouse ua x y
    , uaClick    = \x   -> uaClick uaua x     >>= \ua -> uaClick ua x
    , uaUnclick  = \x   -> uaUnclick uaua x   >>= \ua -> uaUnclick ua x
    , uaScroll   = \x y -> uaScroll uaua x y  >>= \ua -> uaScroll ua x y }

actOnWidgets :: IORef [Widget] -> UserActions (IO ())
actOnWidgets ref = dynamicActions ok where
  ok :: UserActions (IO (UserActions (IO ())))
  ok = pure g
  g :: IO (UserActions (IO ()))
  g = do
    acts <- fmap (map widgetActions) (readIORef ref)
    let noop = return ()
    return $ foldl (liftA2 (>>)) (pure noop) acts
    
makeRoot :: [Widget] -> IO (UserActions (IO ()))
makeRoot ws = do
  var <- newIORef ws
  return $ actOnWidgets var
-}






data MainLoop = MainLoop
  { mlClock :: IO Int
  , mlDoEvents :: IO ()
  , mlDead :: IO Bool
  , mlRender :: IO ()
  , mlDoTick :: IO ()
  }

mainLoop :: MainLoop -> IO ()
mainLoop md = mlClock md >>= loop 0 where
  loop t prev = do
    Just t0 <- GLFW.getTime
    let dt = 1000 * (t0 - t)
    if dt > 21 then print dt else return ()
--    print ("start of loop t0-t0=", (t0 - t) * 1000)
--    print ("t0",t0)
    mlDoEvents md

    now <- mlClock md
--    let delta = now - prev
--    print ("delta", delta)
--    replicateM_ delta (mlDoTick md)
    mlDoTick md

    Just t1 <- GLFW.getTime
--    print ("t1",t1)
    mlRender md
    Just t2 <- GLFW.getTime
--    print ("t2",t2)

    dead1 <- mlDead md
    let dead2 = False
    when (not (dead1 || dead2)) (loop t0 now)


{-
mainAction win w1 w2 w3 w4 = root (pair a (pair b (pair c d))) where
  root = globalMouseTranslator win . quitter win
--  a = blockKeyboard noop (widgetActions w1)
--  b = blockMouse noop (widgetActions w2)
  a = widgetActions w1
  b = widgetActions w2
  c = widgetActions w3
  d = widgetActions w4
--  noop = return ()
-}

data Layout = Layout
  { logicalWH :: Int2
  , arkanoid  :: Float4
  , debugger  :: Float4 }

main = do
  -- the audio device, currently unused
  (dev,bang) <- setupAudio

  -- setup graphics
  let scale = 1
  -- let there be a window plus graphics
  (win,gfx) <- rainbow "" (floor logicalW) (floor logicalH) scale -- create window load graphics


  -- setup "data sources", some of which backed by IORef
  matsV <- newTVarIO badMaterialColors

  let area = F4 (-160) (-240) 320 480
  (arkV,ap,arkActions) <- newArkVar -- 'model'

  let peek1 tab = let Just (F3 r g b) = M.lookup 'w' tab in r
  let peek2 tab = let Just (F3 r g b) = M.lookup 'w' tab in g
  let peek3 tab = let Just (F3 r g b) = M.lookup 'w' tab in b
  let upd1 x = M.adjust (\(F3 r g b) -> F3 x g b) 'w'
  let upd2 x = M.adjust (\(F3 r g b) -> F3 r x b) 'w'
  let upd3 x = M.adjust (\(F3 r g b) -> F3 r g x) 'w'

  slV1 <- newSliderVar (F4 (-320) (-240) 20 480) matsV peek1 upd1
  slV2 <- newSliderVar (F4 ((-320) + 20) (-240) 20 480) matsV peek2 upd2
  slV3 <- newSliderVar (F4 ((-320) + 40) (-240) 20 480) matsV peek3 upd3

  -- setup widgets
  (wid,act) <- do
    let F4 x y w h = area
    canv <- newCanvas (floor w) (floor h)
    return (someWidget (F2 x y) canv (readIORef arkV) (arkRender matsV ap), arkActions area)

  widgetsV     <- newIORef [("arkanoid", wid)] -- 'views'
  controllersV <- newIORef [("arkanoid", act)] -- 'controllers'

  -- controllers
  -- can create controllers now that reference controllersV
  -- now install arkanoid, sliders to controllersV

  -- the main loop and GLFW callbacks use masterAction
  let getControl = fmap (map snd) (readIORef controllersV)
  let masterAction = globalMouseTranslator win (allTheAction getControl)

  

  --debugger <- makeDebugger gfx (F4 (-320) (-240) 640 480)
  --arena    <- makeArena gfx (F4 (-320) (-240) 640 480)
  --arkanoid <- makeArkanoid gfx (F4 (-160) (-240) 320 480) mats

  --slider1   <- makeSlider gfx (F4 (-320) (-240) 20 480) mats peek1 upd1
  --slider2   <- makeSlider gfx (F4 ((-320) + 20) (-240) 20 480) mats peek2 upd2
  --slider3   <- makeSlider gfx (F4 ((-320) + 40) (-240) 20 480) mats peek3 upd3
  --anim      <- makeAnimation gfx

  --globalMouseTranslator win . quitOnEscape win

  -- let there be a connection between the user and... something
  --jackIn win (mainAction win arkanoid slider1 slider2 slider3)

  -- ok I understand past this point
  jackIn win masterAction

  -- a clock used by the main loop
  ticker <- newTicker

    --blitCanvas gfx canvas (F2 0 (-240))

  --let torch = splat (blackOnWhite Ball) (F4 -0.75 -0.75 1 1)
  let torch0 w = splat (blackOnWhite Ball)
                    w (F4 (-320 + 16) (240 - 16) 32 32)
  let torch1 w = splat (BGColor (C 1 1 1) (Layer (Colorize (C 1 0.5 0) (Trigon (F2 0 0) (F2 0.5 1) (F2 1 0))) (Colorize (C 0 0.5 0) (Axigon (F4 -0.5 -0.5 1 1)))))
                    w (F4 (-320 + 3*16) (240 - 16) 32 32)
  let torch2 w = splat (BGColor (C 1 1 1) $ Colorize (C 0 0.5 1) (Trigon (F2 -0.5 -0.5) (F2 0 0.5) (F2 0.5 -0.5)))
                    w (F4 (-320 + 5*16) (240 - 16) 32 32)
  let torch3 w = splat (BGColor (C 1 1 1) $ Colorize (C 1 0 0) (Minus Ball (Xform (F4 1.25 0 0 1.25) Ball)))
                    w (F4 (-320 + 7*16) (240 - 16) 32 32)

{-
  (pic1,pic2) <- loadExample
  let imglib name = case name of "shovel" -> pic1; "yoshi" -> pic2; _ -> pic2
  let tpd = texParamDefaults
  let shot = example [TexImage (LibraryImg "shovel") tpd, TexImage (LibraryImg "yoshi") tpd]
  img <- imageOneShot imglib 640 480 shot
  JuicyPixels.savePngImage "mypicture.png" img
-}

  dims <- readIORef (gfxPhysicalWindowDimensions gfx)
  torch0 dims
  torch1 dims
  torch2 dims
  torch3 dims

  mainLoop $ MainLoop{
    mlClock=ticker,
    mlDoEvents=GLFW.pollEvents,
    mlDoTick=do
      return (),
--      uaTick masterAction 1,
    mlRender=do
--      clearColorBuffer 0.1 0.1 0.2
--      ws <- readIORef widgetsV
--      forM_ ws (repaintWidget gfx . snd)
--      forM_ (reverse ws) (blitWidget gfx . snd)
--        clearColorBuffer 1 1 0
        --renderOneShot imglib shot
        
        GLFW.swapBuffers win
        return ()
      ,
    mlDead=GLFW.windowShouldClose win
  }

  teardownAudio dev


{-
masterAction :: IORef (UserActions (IO ())) -> UserActions (IO ())
masterAction var = UserActions
  { uaKeydown  = (\x y -> readIORef var >>= \z -> uaKeydown z x y)
  , uaKeyup    = (\x y -> readIORef var >>= \z -> uaKeyup z x y)
  , uaKeyagain = (\x y -> readIORef var >>= \z -> uaKeyagain z x y)
  , uaTyping   = (\x ->   readIORef var >>= \z -> uaTyping z x)
  , uaMouse    = (\x y -> readIORef var >>= \z -> uaMouse z x y)
  , uaClick    = (\x ->   readIORef var >>= \z -> uaClick z x)
  , uaUnclick  = (\x ->   readIORef var >>= \z -> uaUnclick z x)
  , uaScroll   = (\x y -> readIORef var >>= \z -> uaScroll z x y) }
-}

quitter :: GLFW.Window -> UserActions (IO ()) -> UserActions (IO ())
quitter win orig = orig{uaKeydown = g} where
  g key mods = do
    case key of
      KBEscape -> GLFW.setWindowShouldClose win True
      _        -> return ()
    uaKeydown orig key mods

banger :: IO () -> UserActions (IO ()) -> UserActions (IO ())
banger bang orig = orig{uaKeydown = g} where
  g key mods = do
    case key of
      KBSpace  -> bang
      _        -> return ()
    uaKeydown orig key mods

weird :: IORef (UserActions (IO ())) -> UserActions (IO ()) -> UserActions (IO ())
weird ref orig = orig{uaKeydown = g} where
  g key mods = do
    case key of
      KBNumPlus -> writeIORef ref (pure (pure ()))
      _         -> return ()
    uaKeydown orig key mods

cacheStateHandleAds :: Functor f => IORef a -> (b -> IO ()) -> f (a -> (a,b)) -> f (IO ())
cacheStateHandleAds var h ua = fmap f ua where
  f g = do
    ads <- atomicModifyIORef' var g
    h ads


planB :: a -> UserActions (Maybe a) -> UserActions a
planB d ua = fmap (fromMaybe d) ua

-- mouse actions stop here
blockMouse :: a -> UserActions a -> UserActions a
blockMouse d orig =
  let f _ = d in
  let g _ _ = d in
  orig{uaMouse=g, uaClick=f, uaUnclick=f, uaScroll=g}

-- keyboard stops here
blockKeyboard :: a -> UserActions a -> UserActions a
blockKeyboard d orig =
  let f _ = d in
  let g _ _ = d in
  orig{uaTyping=f, uaKeydown=g, uaKeyup=g, uaKeyagain=g}

-- simply control two things at once
pair :: UserActions (IO ()) -> UserActions (IO ()) -> UserActions (IO ())
pair = liftA2 (>>)

allTheAction :: IO [UserActions (IO ())] -> UserActions (IO ())
allTheAction getCtrl =
  let fff  = fmap . fmap . fmap
      ff   = fmap . fmap
      noop = return ()
  in flatUA $ (fff (foldr (>>) noop) . ff sequenceA) (pure getCtrl)


-- glfw gives mouse coords [0,480] x [0,640] where y=0 is at the top of the window
-- we want [-320,320] x [-240,240] where 0 is in the middle of the window and y=-240 is at the bottom
-- also factor in scale factor so physical size increase doesn't change these numbers
globalMouseTranslator :: GLFW.Window -> UserActions (IO ()) -> UserActions (IO ())
globalMouseTranslator win ua@UserActions{uaMouse=orig} = ua{uaMouse=g} where
  g x y = do
    (w,h) <- GLFW.getWindowSize win
    let halfW = fi (w `div` 2)
    let halfH = fi (h `div` 2)
    let xfactor = halfW / (logicalW / 2)
    let yfactor = halfH / (logicalH / 2)
    let x' = (x - halfW) / xfactor
    let y' = (halfH - y) / yfactor
    --print ((x,y),(x',y'))
    orig x' y'

logicalW = 640
logicalH = 480





{-
if actions at some point are directed to "all widgets in this box"
and the renderer looks at "all widgets in this box"
and resources for a widget are only referenced through the widget

then you can add and remove widgets by modifying the box
-}
