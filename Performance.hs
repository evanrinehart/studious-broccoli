{-# LANGUAGE NamedFieldPuns #-}
module Performance where

import Data.IORef
import Data.Foldable
import Control.Exception

import Ticks
import Common

  {-
  that <- readIORef oldTick
  this <- getTick
  let n = this - that
  writeIORef oldTick this
  -}
data PerformanceStuff = PerformanceStuff
  { getTime :: IO Double
  , rPrev :: IORef Double
  , rPC1  :: IORef [Double]
  , rPC2  :: IORef [Double]
  , rMetrics :: IORef (Double,Int)
  }

newPerformanceStuff :: IO Double -> IO PerformanceStuff
newPerformanceStuff gt = do
  now <- gt
  PerformanceStuff <$>
    pure gt <*>
    newIORef now <*>
    newIORef [0] <*>
    newIORef [0] <*>
    newIORef (100,9999)

performanceBracket :: PerformanceStuff -> IO () -> IO ()
performanceBracket (PerformanceStuff{getTime,rPrev,rPC1,rPC2,rMetrics}) action = do

  (_, intraFrameDelta) <- stopwatch action

  prev <- readIORef rPrev
  now <- getTime
  let interFrameDelta = 1000 * (now - prev)
  writeIORef rPrev now

  health <- feedPerformanceComputer rPC1 pc1View interFrameDelta
  armor  <- feedPerformanceComputer rPC2 pc2View intraFrameDelta

  evaluate health
  evaluate armor
  writeIORef rMetrics (health, floor armor)

feedPerformanceComputer :: IORef [Double] -> ([Double] -> a) -> Double -> IO a
feedPerformanceComputer ref f x = do
  xs <- atomicModifyIORef' ref (\xs -> let xs' = take 20 (x:xs) in (xs',xs'))
  return (f xs)

-- for the interframe delta where less than 33ms is interpreted as a hit
pc1View :: [Double] -> Double
pc1View xs = 100 * hits / (hits + misses) where
  hits :: Double
  hits = foldl' (+) 0 (map (\x -> if x/33 < 1 then 1 else 0) xs)
  misses :: Double
  misses = foldl' (+) 0 (map (\x -> ffloor (x/33)) xs)

-- for health of the main loop body which ought to take at most 16.666ms
-- formula: divide 16.666 by the measured time, log10, avg, exp10
pc2View :: [Double] -> Double
pc2View = (\x -> 10000 - x) . (*10000) . avg . map (\x -> x / 16.666) where
  avg xs = sum xs / fi (length xs)
