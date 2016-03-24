module ImageWriter
    (
      writeImage
    ) where

import qualified Codec.Picture    as Pic
import Data.Array.Repa
import Data.Word (Word8)

type Width  = Int
type Height = Int
type Triple a = (a, a, a)
type PixelFromCoordFunc = Int -> Int -> Triple Word8

writeImage :: FilePath -> PixelFromCoordFunc -> Width -> Height -> IO ()
writeImage fp pixelFromCoords width height =
     let genFunc :: Array U DIM2 (Triple Word8) -> Int -> Int -> Pic.PixelRGB8
         genFunc arr x y = let (r, g, b) = unsafeIndex arr (Z :. y :. x :: DIM2)
                           in Pic.PixelRGB8 r g b

     in do arr <- buildArray pixelFromCoords width height
           Pic.writePng fp $ Pic.generateImage (genFunc arr) width height


buildArray :: PixelFromCoordFunc -> Width -> Height -> IO (Array U DIM2 (Triple Word8))
buildArray pfc width height =
    let arrShape = Z :. height :. width

        f (Z :. y :. x) = pfc x (height - 1 - y)

    in computeP $ fromFunction arrShape f
