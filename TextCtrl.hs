module TextCtrl where

import Graphics.UI.GLFW

import Event

data TextCtrl
  = PutChar Char
  | Backspace
  | MoveLeft
  | MoveRight
  | MoveUp
  | MoveDown
  | PageUp
  | PageDown
  | Delete
  | Tab
  | Home
  | End
  | Enter

parseTextCtrl :: Event -> Maybe TextCtrl
parseTextCtrl e = go where
  f k = case k of
    Key'Backspace -> Just Backspace
    Key'Left -> Just MoveLeft
    Key'Right -> Just MoveRight
    Key'Up -> Just MoveUp
    Key'Down -> Just MoveDown
    Key'PageUp -> Just PageUp
    Key'PageDown -> Just PageDown
    Key'Delete -> Just Delete
    Key'Tab -> Just Tab
    Key'Home -> Just Home
    Key'End -> Just End
    Key'Enter -> Just Enter
    _ -> Nothing
  go = case e of
    Keyboard k state mods n -> case state of
      KeyState'Pressed -> f k
      KeyState'Repeating -> f k
      _ -> Nothing
    Typing c -> Just (PutChar c)
    _ -> Nothing
