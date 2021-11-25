module Common where

import Data.Vector.Storable as V (Vector, fromList)
import Graphics.GL

newtype Glyph = Glyph Int

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

{-  a01(-1, 1)   b11(1, 1)
 -  c00(-1,-1)   d10(1,-1)  -}
tileData :: Vector Float
tileData =
  let a = [-1,  1, 0, 1] in
  let b = [ 1,  1, 1, 1] in
  let c = [-1, -1, 0, 0] in
  let d = [ 1, -1, 1, 0] in
  V.fromList (concat [c,d,b,b,a,c])

data ULegend1 = UL1
  { ul1WinWH :: GLint
  , ul1SrcXY :: GLint
  , ul1SrcWH :: GLint
  , ul1DstXY :: GLint
  , ul1DstWH :: GLint }
