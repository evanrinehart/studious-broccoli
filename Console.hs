module Console where

import Control.Monad.RWS
import Data.Maybe

import qualified Graphics.UI.GLFW as GLFW

import Common 
import Event
import U

import TextCtrl

type ConsoleM = RWS Int (Maybe String) (Int,String)

cmdLine :: Int -> E [Event] -> (E (Int,String), E String)
cmdLine curMax rawE = (updateE, commandE) where
  updateE = fmap fst aftermathE
  commandE = filterMap snd aftermathE
  aftermathE :: E ((Int,String),Maybe String)
  aftermathE = snap (runConsole curMax) inputE selfV -- (potential output, new state)
  inputE = filterMap cook rawE -- will select at most 1 event and drop the others
  cook = listToMaybe . catMaybes . map parseTextCtrl
  selfV = hold (0,"") updateE

nonEmpty [] = Nothing
nonEmpty xs = Just xs

-- RWS subprogram for the console
runConsole :: Int -> TextCtrl -> (Int,String) -> ((Int,String),Maybe String)
runConsole curMax ctrl s@(i,buf) = execRWS go curMax (i,buf) where
  go = case ctrl of
    PutChar c -> do
      insert c
      moveRight
    Backspace -> do
      moveLeft
      erase
    MoveLeft -> moveLeft
    MoveRight -> moveRight
    Home -> goHome
    End -> goEnd
    Enter -> do
      execute buf
      returnAndClearAll
    _ -> return ()

goHome = modify (\(i,buf) -> (0, buf))
goEnd = do
  l <- bufLen
  modify (\(i,buf) -> (l, buf))

insert :: Char -> ConsoleM ()
insert c = modify (\(i,buf) -> (i,insertC c i buf))

erase :: ConsoleM ()
erase = modify (\(i,buf) -> (i, deleteC i buf))

putC :: Char -> Int -> String -> String
putC c 0 "" = [c]
putC c 0 (s:ss) = c:ss
putC c i ""     = ' ' : putC c (i-1) ""
putC c i (s:ss) =   s : putC c (i-1) ss

deleteC :: Int -> String -> String
deleteC 0 (c:cc) = cc
deleteC i (c:cc) = c : deleteC (i-1) cc
deleteC i ""     = ""

insertC :: Char -> Int -> String -> String
insertC c 0 "" = [c]
insertC c 0 ss = c:ss
insertC c i ""     = ' ' : insertC c (i-1) ""
insertC c i (s:ss) =   s : insertC c (i-1) ss

bufLen :: ConsoleM Int
bufLen = gets (length . snd)

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse where
  isSpace = (==' ')

execute :: String -> ConsoleM ()
execute cmd = tell (Just (trim cmd))

moveLeft :: ConsoleM ()
moveLeft = modify $ \(i,buf) -> (max 0 (i-1), buf)

moveRight :: ConsoleM ()
moveRight = do
  l <- gets (length . snd)
  modify $ \(i,buf) -> (min l (i+1), buf)

returnAndClearAll = modify (\(i,buf) -> (0,""))


{-
-- sort through GLFW stuff
parseRaw ev = case ev of
  Keyboard GLFW.Key'Left GLFW.KeyState'Pressed _      -> Just MoveLeft
  Keyboard GLFW.Key'Right GLFW.KeyState'Pressed _     -> Just MoveRight
  Keyboard GLFW.Key'Backspace GLFW.KeyState'Pressed _ -> Just Backspace
  Keyboard GLFW.Key'Enter GLFW.KeyState'Pressed _     -> Just Enter
  Keyboard GLFW.Key'Home GLFW.KeyState'Pressed _      -> Just Home
  Keyboard GLFW.Key'End GLFW.KeyState'Pressed _       -> Just End
  Typing c                                            -> Just (CharInput c)
  _                                                   -> Nothing
-}

