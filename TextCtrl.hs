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
parseTextCtrl (KeyboardDown k _) = hmm k
parseTextCtrl (KeyboardAgain k _) = hmm k
parseTextCtrl (Typing c) = Just (PutChar c)

hmm k = case k of
  KBBackspace -> Just Backspace
  KBLeft -> Just MoveLeft
  KBRight -> Just MoveRight
  KBUp -> Just MoveUp
  KBDown -> Just MoveDown
  KBPageUp -> Just PageUp
  KBPageDown -> Just PageDown
  KBDelete -> Just Delete
  KBTab -> Just Tab
  KBHome -> Just Home
  KBEnd -> Just End
  KBEnter -> Just Enter
  _ -> Nothing
