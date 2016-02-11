module Main where

import Ray
import Control.Monad (when)
import qualified System.Directory as Dir
import qualified Codec.Picture as Pic

dataDir :: FilePath
dataDir = "./rayt-imgs"

main :: IO ()
main = do setupDataDirectory

          say "Generating orthographic scene with spheres and triangles."
          orthoScene

          say "Generating perspective scene with same geometry."
          perspScene

          say "Generating second perspective scene."
          perspScene2

          say "Generating single-ray per pixel"
          singleRay

          say "Generating multi-jittered sampling"
          multiRay

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
      sphere2 = Sphere (V   3    3  (-4)) 1
      sphere3 = Sphere (V (-3) (-3) (-3)) 1.3
      sphere4 = Sphere (V   3  (-3) (-6)) 1.5
      triang  = Triangle (V 0 0 (-1)) (V 1 1 (-2)) (V (-1) 1 (-1.5))

      obj1    = Obj sphere1 $ CheckerBoard red blue
      obj2    = Obj sphere2 $ SolidColor blue
      obj3    = Obj sphere3 $ SolidColor green
      obj4    = Obj sphere4 $ SolidColor red
      obj5    = Obj triang  $ SolidColor yellow

      light1  = DirectionalLight 1 $ V 0 0 1
      light2  = DirectionalLight 1 $ normalize $ V 1 0 0.4

      scene1  = Scene [obj1, obj2, obj3, obj4, obj5] [light1] black
      scene2  = Scene [obj1] [light2] black

      eye1    = V 0 0 1
      lookAt1 = V 0 0 (-1)
      up1     = V 0 1 0
      cam1    = Camera eye1 lookAt1 up1 1 512 512 0.005

      eye2    = V 1 6 (-0.9)
      lookAt2 = normalize $ V (-0.2) (-4) (-0.9)
      up2     = normalize $ V 0.2 0.9 (-4)
      cam2    = cam1{position = eye2, lookAt = lookAt2, up = up2}

      eye3 = V (-3) 3 (-4.7)
      cam3 = Camera eye3 lookAt1 up1 512 512 512 6

      orthoScene  = mkImage "ortho" scene1 cam1{cellLength = 0.03} orthographicRayGen

      perspScene  = mkImage "persp1" scene1 cam1 perspectiveRayGen

      perspScene2 = mkImage "persp2" scene1 cam2 perspectiveRayGen

      singleRay   = mkImage "single" scene2 cam3 perspectiveRayGen

      multiRay    = mkImage "multi" scene2 cam3 (multiJitteredRayGen 3)


-- With all the info, make the image and save it to disk
mkImage :: FilePath -> Scene -> Camera -> RayGenFunc -> IO ()
mkImage fp scene cam rgf =
    do let pixelFunc = raycast scene cam rgf

           hght = height cam

           genFunc x y = let (r,g,b) = toRGBPixel (pixelFunc x (hght-1-y))
                         in Pic.PixelRGB8 r g b

           image = Pic.generateImage genFunc (width cam) hght

       Pic.writePng (dataDir ++ "/" ++ fp ++ ".png") image
