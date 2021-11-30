module Event where

import qualified Graphics.UI.GLFW as GLFW

data Event
  = WinSize Int Int
  | Typing Char
  | Keyboard GLFW.Key GLFW.KeyState GLFW.ModifierKeys Int
  | MouseClick GLFW.MouseButton GLFW.MouseButtonState
  | MouseMotion Float Float
  | Scroll Float Float
      deriving Show

-- a mini app is like... a rectangular box that gets events
-- has internal state, a can paint itself to a texture if requested.
-- and it may animate

data MiniApp = MiniApp
  { maPoke    :: Event -> IO ()
  , maTime    :: Int -> IO ()
  , maShow    :: IO () }

-- if the event has coordinates, move by some shift
-- shiftEvent :: Float2 -> Event -> Event
