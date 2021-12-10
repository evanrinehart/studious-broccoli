module Event where

import Graphics.UI.GLFW as GLFW

data Event
  = WinSize Int Int
  | Typing Char
  | KeyboardDown KB [ModKey]
  | KeyboardUp KB [ModKey]
  | KeyboardAgain KB [ModKey]
  | MouseClick Int
  | MouseUnclick Int
  | MouseMotion Float Float
  | Scroll Float Float
      deriving Show

data KB
  = KBLetter Char
  | KBEscape
  | KBBackspace
  | KBLeft | KBRight | KBUp | KBDown
  | KBPageUp | KBPageDown
  | KBDelete | KBHome | KBEnd
  | KBEnter | KBTab
  | KBOther String
      deriving Show

data ModKey = CtrlKey | AltKey | ShiftKey deriving Show

translateGLFWKey :: GLFW.Key -> GLFW.KeyState -> GLFW.ModifierKeys -> Event
translateGLFWKey k state mods = f state where
  f KeyState'Pressed   = KeyboardDown  (g k) []
  f KeyState'Released  = KeyboardUp    (g k) []
  f KeyState'Repeating = KeyboardAgain (g k) []
  g k = case k of
    Key'Backspace -> KBBackspace
    Key'Left -> KBLeft
    Key'Right -> KBRight
    Key'Up -> KBUp
    Key'Down -> KBDown
    Key'PageUp -> KBPageUp
    Key'PageDown -> KBPageDown
    Key'Delete -> KBDelete
    Key'Tab -> KBTab
    Key'Home -> KBHome
    Key'End -> KBEnd
    Key'Enter -> KBEnter
    Key'Escape -> KBEscape
    _ -> KBOther (show k)

isEscape :: Event -> Bool
isEscape (KeyboardDown KBEscape _) = True
isEscape _ = False
