module Console where

import Control.Monad (void)
import Buffer2D

import Common hiding (Rect)

type Frame = Rect Int

data Console = Console
  { consoleBuf :: Buffer2D Glyph
  , consoleWin :: Rect Int
  , consoleCurI :: !Int
  , consoleCurJ :: !Int
  , consoleCmd :: String }

modifyBuf f con = con { consoleBuf = f (consoleBuf con) }
modifyWin f con = con { consoleWin = f (consoleWin con) }
modifyCmd f con = con { consoleCmd = f (consoleCmd con) }

empty :: Int -> Int -> Console
empty w h = Console (blank (Glyph 32)) (Rect 0 0 w h) 0 0 ""

burn :: Glyph -> Int -> Int -> Console -> Console
burn c x y = modifyBuf (putItem c x y)

linefeed :: Console -> Console
linefeed = modifyWin (\(Rect l t w h) -> Rect l (t+1) w h)

key :: Char -> Console -> Console
key '\b' = modifyCmd (drop 1)
key c    = modifyCmd (c:)

eachCell :: Monad m => Console -> (Int -> Int -> Glyph -> m ()) -> m ()
eachCell con f = foldBuffer win (\i j c k -> f i j c >> k) noop buf where
  win = consoleWin con
  buf = consoleBuf con
  noop = return ()

burns :: [(Glyph,Int,Int)] -> Console -> Console
burns xs con = foldr (\(c,x,y) -> burn c x y) con xs

put :: Glyph -> Console -> Console
put c (Console buf w@(Rect wi wj ww wh) ci cj cmd) = Console buf' w' ci' cj' cmd where
  buf' = putItem c ci cj buf
  (ci',cj',w') = if ci < wi + ww - 1
    then (ci+1, cj, w) 
    else (0, cj+1, Rect wi (wj+1) ww wh)

puts :: [Glyph] -> Console -> Console
puts cs console = foldr put console cs
