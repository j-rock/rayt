module ImageWriter
    (
      writeImage
    ) where

import qualified Codec.Picture    as Pic
import Data.Word (Word8)

import qualified Data.Vector.Mutable as MVector
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector as Vector
import Data.Vector (Vector)

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.MVar as MVar
import Control.Concurrent.MVar (MVar)
import qualified Control.Monad as Monad

type Width  = Int
type Height = Int
type Triple a = (a, a, a)
type PixelFromCoordFunc = Int -> Int -> Triple Word8

writeImage :: FilePath -> PixelFromCoordFunc -> Width -> Height -> IO ()
writeImage fp pixelFromCoords width height =
     let genFunc :: Vector (Triple Word8) -> Int -> Int -> Pic.PixelRGB8
         genFunc arr x y = let index     = y * width + x
                               (r, g, b) = Vector.unsafeIndex arr index
                           in Pic.PixelRGB8 r g b

     in do arr <- buildArray pixelFromCoords width height
           Pic.writePng fp $ Pic.generateImage (genFunc arr) width height


buildArray :: PixelFromCoordFunc -> Width -> Height -> IO (Vector (Triple Word8))
buildArray pfc width height =
    let pfc' i = let (y, x) = i `divMod` width
                 in pfc x (height - 1 - y)

        len = width * height

        forkWorker v (s, e) = Concurrent.forkIO . worker v pfc' s e

    in do numWorkers <- Concurrent.getNumCapabilities
          putStrLn $ "Workers: " ++ show numWorkers
          vec <- MVector.new len

          let bounds = genBounds numWorkers len
              ((s1, e1), remBounds) = (head bounds, tail bounds)
              -- want the main thread to do the first interval

          signals <- mapM (const MVar.newEmptyMVar) bounds
          Monad.zipWithM_ (forkWorker vec) remBounds (tail signals)
          worker vec pfc' s1 e1 (head signals)
          Monad.forM_ signals MVar.takeMVar

          Vector.unsafeFreeze vec


genBounds :: Int -> Int -> [(Int, Int)]
genBounds nBounds len =
    let (base, remainder) = len `divMod` nBounds

        go 1 s _ = [(s, len - 1)]
        go i s r = (s, end) : go (i-1) (end+1) (r-1)
          where end = s + base + (if r > 0 then 0 else (-1))

    in go nBounds 0 remainder


worker :: IOVector (Triple Word8) -> (Int -> Triple Word8) -> Int -> Int -> MVar () -> IO ()
worker vec pfc start end signal =
    let write :: Int -> IO ()
        write i = MVector.unsafeWrite vec i $! pfc i

        go i|i > end   = MVar.putMVar signal ()
        go i|otherwise = write i >> go (i+1)

    in go start
