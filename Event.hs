module Event where

import qualified Graphics.UI.GLFW as GLFW

data Event
  = WinSize Int Int
  | Typing Char
  | Keyboard GLFW.Key GLFW.KeyState Int
  | MouseClick GLFW.MouseButton GLFW.MouseButtonState
  | MouseMotion Float Float
  | Scroll Float Float
      deriving Show
