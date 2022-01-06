module Slider where

import Control.Concurrent.STM
import Data.IORef

import Debug.Trace

import Common
import Widget
import Rainbow
import Event

data Slider = Slider
  { slXYWH  :: !Float4
  , slMouse :: !Float2
  , slDrag  :: !Bool
  , slValue :: !Float }
      deriving Show

sliderActions :: UserActions (Slider -> Slider)
sliderActions =
  idAction {
    uaMouse = sliderMouse,
    uaClick = sliderClick,
    uaUnclick = sliderUnclick,
    uaScroll = sliderScroll
  }

-- slider control notes
-- each slider is backed by and effectively controls its own IORef
-- we can form a data source from that IORef, basically making the slider a data source.
-- the mouse...


sliderMouse :: Float -> Float -> Slider -> Slider
sliderMouse mx my sl
  | slDrag sl = sl { slMouse = F2 mx my, slValue = sliderMap sl my }
  | otherwise = sl { slMouse = F2 mx my }

sliderClick :: Int -> Slider -> Slider
sliderClick but sl@(Slider box mouse@(F2 mx my) _ _)
  | but == 0 && mouse `inside` box = sl { slDrag = True, slValue = sliderMap sl my }
  | otherwise = sl

sliderUnclick :: Int -> Slider -> Slider
sliderUnclick but sl
  | but == 0  = sl { slDrag = False }
  | otherwise = sl

sliderScroll :: Float -> Float -> Slider -> Slider
sliderScroll dx dy sl@Slider{slDrag=False,slValue=y} = sl { slValue = y + dy }
sliderScroll dx dy sl@Slider{slDrag=True}            = sl

inside :: Float2 -> Float4 -> Bool
inside (F2 a b) (F4 _ _ w h) = not (a < -w/2 || b < -h/2 || a > w/2 || b > h/2)

-- x minimum = 0, x maximum = 1
sliderMap sl y =
  let F4 _ base _ h = slXYWH sl
  in max 0 (min 1 ((y-base) / h))

sliderWidget :: IORef Slider -> Float2 -> Canvas -> Widget
sliderWidget var xy canvas = someWidget xy canvas (readIORef var) renderSlider




makeSlider :: GFX -> Float4 -> TVar a -> (a -> Float) -> (Float -> a -> a) -> IO Widget
makeSlider gfx (F4 x y w h) var peek update = do
  canvas <- newCanvas (floor w) (floor h)

  iv <- atomically (fmap peek (readTVar var))

  let area = F4 x y w h
  ref <- newIORef (Slider area (F2 0 0) False iv)

  let k sl = atomically (readTVar var >>= writeTVar var . update (slValue sl))

  return $ Widget{
    widgetCanvas  = canvas,
    widgetRepainter = Repainter (readIORef ref) renderSlider,
    widgetXY = F2 x y

{-
    widgetActions = (shiftMouse area . updateCachedStateThen ref k) sliderActions,
    widgetRepaint = readIORef ref >>= renderSlider gfx canvas,
    widgetArea    = area
-}
  }

-- controller? shiftMouse <- use state cache <- update function
-- (shiftMouse area . updateCachedStateThen ref k) sliderActions

newSliderVar :: Float4 -> TVar a -> (a -> Float) -> (Float -> a -> a) -> IO (IORef Slider)
newSliderVar (F4 x y w h) var peek update = do
  iv <- atomically (fmap peek (readTVar var))
  let area = F4 x y w h
  newIORef (Slider area (F2 0 0) False iv)

renderSlider :: GFX -> Canvas -> Slider -> IO ()
renderSlider gfx canvas sl = do
  clearCanvas (F3 0.1 0.1 0.2) canvas
  let F4 _ _ w h = slXYWH sl
  let y = slValue sl * h - h / 2
  withBlock gfx canvas $ \torch -> do
    torch (F4 0 (-h/2) 1 h) (F3 0.3 0.3 0.6)
    torch (F4 (-w/2) (y-2) w 5) (F3 0.3 0.3 0.6)
