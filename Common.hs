module Common where

import Data.Vector.Storable as V
import Graphics.GL

newtype Glyph = Glyph Int

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

{-  01(-1, 1)   11(1, 1)
 -  00(-1,-1)   10(1,-1)  -}
tileData :: Vector Float
tileData = V.fromList
  [-1, -1, 0, 0
  , 1, -1, 1, 0
  , 1,  1, 1, 1
  , 1,  1, 1, 1
  ,-1,  1, 0, 1
  ,-1, -1, 0, 0]

data ULegend1 = UL1
  { ul1WinWH :: GLint
  , ul1SrcXY :: GLint
  , ul1SrcWH :: GLint
  , ul1DstXY :: GLint
  , ul1DstWH :: GLint }
