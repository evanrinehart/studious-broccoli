module MiniApp where

import Event
import Glue
import Common

-- a mini app is like... a rectangular box that gets events
-- has internal state, a can paint itself to a texture if requested.
-- and it may animate

data MiniApp = MiniApp
  { maPoke    :: Event -> IO ()
  , maTime    :: Int -> IO ()
  , maShow    :: IO () }

