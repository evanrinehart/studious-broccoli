module Console where

import Control.Monad.RWS
import Data.Maybe

import Common 
import Event
import U

import TextCtrl

type ConsoleM = RWS () (Maybe String) (Int,String)

-- when an event makes it through, update the cmdLine, notify
-- with current state, and a second event occurs on command entry.
cmdLine :: E [Event] -> (E (Int,String), E String)
cmdLine rawE = (updateE, commandE) where
  updateE :: E (Int,String)
  updateE = fmap fst aftermathE

  commandE :: E String
  commandE = filterMap snd aftermathE

  aftermathE :: E ((Int,String),Maybe String)
  aftermathE = snap runControl inputE selfV -- (potential output, new state)

  inputE :: E TextCtrl
  inputE = inject Backspace (\x y -> y) $ filterMap cook rawE
  cook = listToMaybe . catMaybes . map parseTextCtrl

  selfV :: V (Int,String)
  selfV = hold (0,"") updateE

-- RWS subprogram for the console
runControl :: TextCtrl -> (Int,String) -> ((Int,String),Maybe String)
runControl ctrl (i,buf) = execRWS go () (i,buf) where
  go = case ctrl of
    PutChar c -> insert c >> moveRight
    Backspace -> moveLeft >> erase
    MoveLeft  -> moveLeft
    MoveRight -> moveRight
    Home -> goHome
    End  -> goEnd
    Enter -> execute buf >> returnAndClearAll
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

