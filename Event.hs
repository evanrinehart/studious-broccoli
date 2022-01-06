{-# LANGUAGE DeriveFunctor #-}
module Event where

import Control.Monad.RWS (RWS, execRWS)
import Data.IORef

import Graphics.UI.GLFW as GLFW

import Common

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
  , uaScroll   :: Float -> Float -> a
  , uaTick     :: Float -> a }
      deriving Functor

noAction :: Monad m => UserActions (m ())
noAction = pure (return ())

idAction :: UserActions (a -> a)
idAction = pure id

cachedState :: Functor f => IORef a -> f (a -> a) -> f (IO ())
cachedState var ua = fmap f ua where
  f g = modifyIORef' var g

cachedStateIgnore :: Functor f => IORef a -> f (a -> (a,b)) -> f (IO ())
cachedStateIgnore var ua = fmap f ua where
  f g = do
    _ <- atomicModifyIORef' var g
    return ()

updateCachedStateThen :: Functor f => IORef a -> (a -> IO ()) -> f (a -> a) -> f (IO ())
updateCachedStateThen var k ua = fmap f ua where
  f g = do
    modifyIORef' var g
    x <- readIORef var
    k x
  

-- each widget needs their mouse positions shifted according to canvas location
shiftMouse :: Float4 -> UserActions (IO ()) -> UserActions (IO ())
shiftMouse (F4 x y w h) orig = orig{ uaMouse = g } where
  shift (F2 mx my) = F2 (mx - (x + w/2)) (my - (y + h/2))
  g x y = do
    let F2 x' y' = shift (F2 x y)
    uaMouse orig x' y'


flatUA :: UserActions (IO (UserActions (IO a))) -> UserActions (IO a)
flatUA ua =
  let f l1 l2 = ((>>=) . l1 ua) <*> flip l2
      g l1 l2 x y = l1 ua x y >>= \ua' -> l2 ua' x y in
  UserActions
  { uaKeydown  = g uaKeydown uaKeydown
  , uaKeyup    = g uaKeyup uaKeyup
  , uaKeyagain = g uaKeyagain uaKeyagain
  , uaTyping   = f uaTyping uaTyping
  , uaMouse    = g uaMouse uaMouse
  , uaClick    = f uaClick uaClick
  , uaUnclick  = f uaUnclick uaUnclick
  , uaScroll   = g uaScroll uaScroll
  , uaTick     = f uaTick uaTick
  }


instance Applicative UserActions where
  pure x =
    let f _ = x in
    let g _ _ = x in
    UserActions g g g f g f f g f
  UserActions f1 f2 f3 f4 f5 f6 f7 f8 f9 <*> UserActions g1 g2 g3 g4 g5 g6 g7 g8 g9 =
    let h1 x y = f1 x y (g1 x y)
        h2 x y = f2 x y (g2 x y)
        h3 x y = f3 x y (g3 x y)
        h4 = f4 <*> g4
        h5 x y = f5 x y (g5 x y)
        h6 = f6 <*> g6
        h7 = f7 <*> g7
        h8 x y = f8 x y (g8 x y)
        h9 = f9 <*> g9
    in UserActions h1 h2 h3 h4 h5 h6 h7 h8 h9

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
