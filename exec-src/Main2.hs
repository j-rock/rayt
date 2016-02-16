module Main where

import qualified Codec.Picture    as Pic
import           Control.Monad    (when)
import           Ray
import qualified System.Directory as Dir

-- This is the directory where the output images will be (re)placed
dataDir :: FilePath
dataDir = "./rayt-imgs"

-- Entry point to program
main :: IO ()
main = do setupDataDirectory

          say "Generating scene"
          theScene

          say "Generating good scene"
          theScene2

          say "That's all folks!"

  where
      say = putStrLn

      setupDataDirectory = do
          exists <- Dir.doesDirectoryExist dataDir
          when exists $ do
              say "Removing data directory"
              Dir.removeDirectoryRecursive dataDir
          say $ "Creating data directory: " ++ dataDir
          Dir.createDirectoryIfMissing False dataDir

      sphere1 = Sphere (V (-3)   3  (-6)) 1.1
      sphere2 = Sphere (V (-3) (-3) (-3)) 1.3
      sphere3 = Sphere (V   3  (-3) (-6)) 1.5
      triang  = Triangle (V 0 0 (-1)) (V 1 1 (-2)) (V (-1) 1 (-1.5))
      plane   = planeFromNormalAndPoint n p0
        where n  = V 0 1 0.2
              p0 = V 0 (-10) 0

      obj1    = Obj sphere1 $ CheckerBoard blue white
      obj2    = Obj plane   $ CheckerBoard green black
      obj3    = Obj sphere2 $ SolidColor green
      obj4    = Obj sphere3 $ SolidColor red
      obj5    = Obj triang  $ CheckerBoard yellow black

      light1  = DirectionalLight 0.8 $ V (-1) 0 (-1)

      scene1  = Scene [obj1, obj2, obj3, obj4, obj5] [light1] (rgb 0.1 0.1 0.2)

      eye1    = V 0 0 1
      lookAt1 = V 0 0 (-1)
      up1     = V 0 1 0
      cam1    = Camera eye1 lookAt1 up1 1 512 512 0.005

      theScene   = mkImage "moire"         scene1 cam1 perspectiveRayGen
      theScene2  = mkImage "multijittered" scene1 cam1 (multiJitteredRayGen 5)


-- With all the info, make the image and save it to disk
mkImage :: FilePath -> Scene -> Camera -> RayGenFunc -> IO ()
mkImage fp scene cam rgf =
    do let pixelFunc = raycast scene cam rgf

           hght = height cam

           genFunc x y = let (r,g,b) = toRGBPixel (pixelFunc x (hght-1-y))
                         in Pic.PixelRGB8 r g b

           image = Pic.generateImage genFunc (width cam) hght

       Pic.writePng (dataDir ++ "/" ++ fp ++ ".png") image
