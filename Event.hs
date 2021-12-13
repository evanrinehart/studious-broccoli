{-# LANGUAGE DeriveFunctor #-}
module Event where

import Control.Monad.RWS (RWS, execRWS)

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
  | KBSpace | KBNumPlus
  | KBOther String
      deriving Show

data ModKey = CtrlKey | AltKey | ShiftKey | OtherMod String deriving Show

data UserActions a = UserActions
  { uaKeydown  :: KB -> [ModKey] -> a
  , uaKeyup    :: KB -> [ModKey] -> a
  , uaKeyagain :: KB -> [ModKey] -> a
  , uaTyping   :: Char -> a
  , uaMouse    :: Float -> Float -> a
  , uaClick    :: Int -> a
  , uaUnclick  :: Int -> a
  , uaScroll   :: Float -> Float -> a }
      deriving Functor

instance Applicative UserActions where
  pure x =
    let f _ = x in
    let g _ _ = x in
    UserActions g g g f g f f g
  UserActions f1 f2 f3 f4 f5 f6 f7 f8 <*> UserActions g1 g2 g3 g4 g5 g6 g7 g8 =
    let h1 x y = f1 x y (g1 x y)
        h2 x y = f2 x y (g2 x y)
        h3 x y = f3 x y (g3 x y)
        h4 = f4 <*> g4
        h5 x y = f5 x y (g5 x y)
        h6 = f6 <*>  g6
        h7 = f7 <*>  g7
        h8 x y = f8 x y (g8 x y)
    in UserActions h1 h2 h3 h4 h5 h6 h7 h8

runUA :: UserActions (RWS () w s a) -> UserActions (s -> (s,w))
runUA act = fmap (\act s -> execRWS act () s) act

translateGLFWMods :: GLFW.ModifierKeys -> [ModKey]
translateGLFWMods ms = (map fst . filter snd) hay where
  hay = [(ShiftKey, modifierKeysShift ms)
        ,(CtrlKey, modifierKeysControl ms)
        ,(AltKey, modifierKeysAlt ms)
        ,(OtherMod "Super", modifierKeysSuper ms)]

translateGLFWKey :: GLFW.Key -> KB
translateGLFWKey k = case k of
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
  Key'Space -> KBSpace
  Key'PadAdd -> KBNumPlus
  _ -> KBOther (show k)

isEscape :: Event -> Bool
isEscape (KeyboardDown KBEscape _) = True
isEscape _ = False
