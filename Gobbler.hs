-- the gobbler!!! haha!
{-# LANGUAGE NamedFieldPuns #-}
module Gobbler where

import System.Directory

import System.IO.Unsafe

import Data.Maybe
import Data.Char
import Data.IORef
import Control.Monad
import Control.Exception

import Common
import U
import PrinterPaper

import Ticks


--box = 33 + 40 * 35 + 30
--darkbox = 33 + 40 * 35 + 29

type CmdEdit = E (Int,String)
type CmdExec = E String

-- two input events
-- 1. the current state of the buffer is delivered, suggest you re-render the picture
-- 2. command line entered, suggest running the command someho
hatchGobbler :: CmdEdit -> CmdExec -> PrinterPaper -> IO (IO ())
hatchGobbler e1 e2 pp = do
  ref1 <- newIORef e1
  ref2 <- newIORef e2
  return $ gobbler ref1 ref2 pp



gobbler :: IORef CmdEdit -> IORef CmdExec -> PrinterPaper -> IO ()
gobbler e1 e2 pp = do
  ocs1 <- atomicModifyIORef' e1 (\(E (e:ee)) -> (E ee, e))
  ocs2 <- atomicModifyIORef' e2 (\(E (e:ee)) -> (E ee, e))

  let bl = F3 0 0 0
  let wh = F3 1 1 1

  -- ocs1 and ocs2 are Maybe X
  forM_ ocs2 $ \cmd -> do
    --xs <- listDirectory cmd
    clear pp

    burnMany pp (F3 1 1 1) (F3 0 0 0) (I2 0 3) cmd
      
    putStrLn cmd

  case ocs1 of
    Nothing -> return ()
    Just (cur,buf) -> burnCmd pp cur buf

  present pp


{-
data GobblerGroup = GG { ggCid::Int, ggPulse::[IO ()] }


egg :: E a -> (a -> IO ()) -> IO (IO ())
egg e act = do
  ref <- newIORef e
  return $ do
    x <- atomicModifyIORef' e1 (\(E (e:ee)) -> (E ee, e))
    act x
-}


-- new project, can we make a generic gobbler.
-- each "dangling event" E needs some periodic consumer to
--   1. take and discord the current element
--   2. do something with it, optionally
-- the main loop executes each such gobbler each frame (once).

-- if a system with n dangling events is decommissioned, 
-- then those gobblers have to be decommissioned from the
-- basket.
