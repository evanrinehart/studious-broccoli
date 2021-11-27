{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module TheThing where

import Control.Monad
import Data.IORef
import Data.Char

import TextMode
import MiniApp
import Event
import Common

makeTheThing :: TextTool -> IO MiniApp
makeTheThing tool = do
  ref <- newIORef []
  return (theThing tool ref)

theThing :: TextTool -> IORef [Char] -> MiniApp
theThing tool ref = MiniApp {maPoke, maShow} where

  maPoke (Typing c) = modifyIORef ref (c:)
  maPoke _          = return ()

  maShow = tool $ \burn -> do
    s <- fmap reverse $ readIORef ref
    forM_ (zip s [0..]) $ \(c,i) -> do
      burn (Glyph (ord c - 33)) yellow red i 0

  --maTime _ = return ()
