{-# LANGUAGE BangPatterns #-}

module Main where

import qualified ImageWriter
import           Control.Monad    (when)
import           Ray
import qualified System.Directory as Dir

-- This is the directory where the output images will be (re)placed
dataDir :: FilePath
dataDir = "./rayt-imgs"

-- Entry point to program
main :: IO ()
main = do setupDataDirectory

          say "Reading in mesh"
          Right meshObj <- readInObj "./data/bunny.obj" redMat

          say "Generating scene"
          image1 [meshObj]

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

      readInObj fp mat = do res <- readInMesh fp
                            case res of
                              Left err -> say err >> return (Left res)
                              Right m  -> return (Right (Obj (Right m) mat))

      matFromColor v = SolidColor v v
      floorMat = matFromColor (RGB $ V 0.449 0.597 0.84375)
      redMat   = matFromColor red


      floorPlane = planeFromNormalAndPoint (V 0 0 1) (V 0 (-0.0101) (-4))
      floorObj = Obj (Left floorPlane) floorMat

      littleNumSpheres = 100

      spheres = map (\p -> Obj (Left (Sphere p 0.5)) (cl p)) pts
        where rng = [1 .. littleNumSpheres]
              pts = [V x y 0 | x <- rng, y <- rng]
              cl (V x y z)| x + y > 20 = matFromColor red
                          | y + y > 11 = matFromColor blue
                          | otherwise  = matFromColor green

      bSphere = Sphere (V 0.0608 0.189 (-1.168e-1)) 0.04
      bunnySphereObj = Obj (Left bSphere) $ matFromColor yellow

      objects = floorObj:spheres
      -- objects = []

      light1 = directionalLight 0.3 $ V 0 0 (-1)
      light2 = directionalLight 0.4 $ V 0 (-1) 1
      light3 = PointLight 0.01 $ V (-2.68e-2) (9.349e-2) (-7.872e-2)
      light4 = PointLight 0.005 $ V (-1.68e-2) (9.349e-2) (-7.872e-2)
      light5 = directionalLight 0.3 $ V 1 (-1) (-1)
      lts    = [ light3, light5, light2
      -- lts    = [ light3, light1, light2
               ]

      colorDeets = ColorDetails {
                     backgroundColor     = RGB (V 0.5 0.5 0.5)
                   , diffuseCoefficient  = 0.9
                   , specularCoefficient = 1
                   , specularExponent    = 15
                   }

      scene = Scene objects lts colorDeets

      squareSide = 512
      cam1 = Camera {
               position    = V (-1.68e-2) 9.410901e-2 0
             , lookAt      = V 0 (-0.1) (-1)
             , up          = V 0 1 (-0.1)
             , focalLength = 1
             , width       = squareSide
             , height      = squareSide
             , cellLength  = 0.18699 / fromIntegral squareSide
             }

      cam = cam1{ position    = V (littleNumSpheres / 2) (littleNumSpheres / 2) littleNumSpheres
                , lookAt      = V 0 0 (-1)
                , up          = V 0 1 0
                , cellLength  = 0.1
                }

      image1 extraObjs = mkImage "persp" scene cam orthographicRayGen
        where scene' = scene{objs = objs'}
              objs'  = octreeFromObjects $ extraObjs ++ objs scene


-- With all the info, make the image and save it to disk
mkImage :: Intersector i => FilePath -> Scene i -> Camera -> RayGenFunc -> IO ()
mkImage !fp !scene !cam !rgf =
    let pixelFunc !x !y = toRGBPixel $ raycast scene cam rgf x y
        outPath   = dataDir ++ "/" ++ fp ++ ".png"

    in ImageWriter.writeImage outPath pixelFunc (width cam) (height cam)
