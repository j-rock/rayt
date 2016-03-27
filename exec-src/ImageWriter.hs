module ImageWriter
    (
      writeImage
    ) where

import qualified Codec.Picture    as Pic
import Data.Word (Word8)

import qualified Data.Vector
import Data.Vector (Vector)
import qualified Data.Vector.Generic as Vector
import qualified Control.Parallel.Strategies as Strat
import Control.Parallel.Strategies (Strategy, using)
import Control.DeepSeq (NFData)

type Width  = Int
type Height = Int
type Triple a = (a, a, a)
type PixelFromCoordFunc = Int -> Int -> Triple Word8

writeImage :: FilePath -> PixelFromCoordFunc -> Width -> Height -> IO ()
writeImage fp pixelFromCoords width height =
     let genFunc :: Vector (Triple Word8) -> Int -> Int -> Pic.PixelRGB8
         genFunc arr x y = let index = y * width + x
                               (r, g, b) = Data.Vector.unsafeIndex arr index
                           in Pic.PixelRGB8 r g b

     in do let arr = buildArray pixelFromCoords width height
           Pic.writePng fp $ Pic.generateImage (genFunc arr) width height


buildArray :: PixelFromCoordFunc -> Width -> Height -> Vector (Triple Word8)
buildArray pfc width height =
    let pfc' i = let (y, x) = i `divMod` width
                 in pfc x (height - 1 - y)

        vec = Vector.enumFromN 0 (width * height)

    in (pfc' <$> vec) `using` parVector 60

-- #419begin
--   #type=1
--   #src=https://hackage.haskell.org/package/vector-strategies-0.4/docs/src/Data-Vector-Strategies.html
-- #419end
parVector :: (Vector.Vector v a, NFData a) => Int -> Strategy (v a)
parVector n = fmap Vector.fromList . strat . Vector.toList
  where strat = Strat.parListChunk n Strat.rdeepseq
