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

          say "Generating perspective scene"
          perspScene

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

      sphere1 = Sphere (V   (-3) 3  (-4)) 1.1
      sphere2 = Sphere (V   3    3  (-4)) 1
      sphere3 = Sphere (V (-3) (-3) (-3)) 1.3
      sphere4 = Sphere (V   3  (-3) (-6)) 1.5
      sphere5 = Sphere (V (-3) (-2.4) (3)) 1.3

      redMat   = SolidColor red red
      blueMat  = SolidColor blue blue
      greenMat = SolidColor green green

      obj1 = Obj (Left sphere1) redMat
      obj2 = Obj (Left sphere2) blueMat
      obj3 = Obj (Left sphere3) greenMat
      obj4 = Obj (Left sphere4) redMat
      obj5 = Obj (Left sphere5) redMat
      -- objects = [ obj5
      --           ]
      objects = [ obj1
                , obj2
                , obj3
                , obj4
                , obj5
                ]

      light1 = directionalLight 1 $ V 0 0 (-1)
      lts    = [ light1
               ]

      colorDeets = ColorDetails {
                     backgroundColor     = RGB (V 0.5 0.5 0.5)
                   , diffuseCoefficient  = 1
                   , specularCoefficient = 1
                   , specularExponent    = 1
                   }

      scene = Scene objects lts colorDeets

      cam   = Camera {
                position    = V 0 0 1.4
              , lookAt      = V 0 (-0.3) (1)
              , up          = V 0 1 (0.3)
              , focalLength = 1
              , width       = 512
              , height      = 512
              , cellLength  = 0.005
              }

      perspScene  = mkImage "persp" scene cam perspectiveRayGen


-- With all the info, make the image and save it to disk
mkImage :: Intersector i => FilePath -> Scene i -> Camera -> RayGenFunc -> IO ()
mkImage fp scene cam rgf =
    do let pixelFunc = raycast scene cam rgf

           hght = height cam

           genFunc x y = let (r,g,b) = toRGBPixel (pixelFunc x (hght-1-y))
                         in Pic.PixelRGB8 r g b

           image = Pic.generateImage genFunc (width cam) hght

       Pic.writePng (dataDir ++ "/" ++ fp ++ ".png") image
