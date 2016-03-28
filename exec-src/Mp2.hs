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
          Right mesh <- readInMesh "./data/bunny.obj"

          say "Generating scene"
          image1 mesh

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


      matFromColor v = SolidColor v v
      floorMat = matFromColor (RGB $ V 0.449 0.597 0.84375)
      redMat   = matFromColor red


      floorPlane = planeFromUnitNormalAndPoint (V 0 1 0) (V 0 0 0)
      floorObj = Obj (Left floorPlane) floorMat $ mkAffine []

      littleNumSpheres = 5

      spheres = map (\p -> Obj sph (cl p) (trn p)) pts
        where rng = [1 .. littleNumSpheres]
              sph = Left $ Sphere (V 0 0 0) 1
              pts = [V x 0 z | x <- rng, z <- rng]
              trn p = mkAffine [translateA p, scaleA 0.5]
              cl (V x _ z)| x + z > 20 = matFromColor red
                          | z + z > 11 = matFromColor blue
                          | otherwise  = matFromColor green

      objects = []

      light1 = directionalLight 0.1 $ V 0 0 (-1)
      light2 = directionalLight 0.6 $ V 1 (-1) (-1)
      light3 = PointLight 0.3 $ V 1.1 1 1.45
      light4 = PointLight 0.25 $ V 4.1 1 4.45
      light5 = directionalLight 0.3 $ V 1 0 0
      lts    = [
                 -- light1
                 light2
               -- , light3
               -- , light4, light5
               ]

      colorDeets = ColorDetails {
                     backgroundColor     = RGB (V 0.5 0.5 0.5)
                   , ambientCoefficient  = 0.5
                   , ambientIntensity    = 0.4
                   , diffuseCoefficient  = 0.3
                   , specularCoefficient = 0.7
                   , specularExponent    = 10
                   }

      scene = Scene objects lts colorDeets

      cam = cam2{ position    = V (littleNumSpheres / 2) 2 (littleNumSpheres / 2)
                , lookAt      = V 0 (-1) 0
                , up          = V 0 0 1
                , cellLength  = (1.2 * littleNumSpheres) / fromIntegral squareSide
                , focalLength = 0.1
                }

      squareSide = 512
      cam2 = Camera {
               position    = V (-1)  6 (-1)
             , lookAt      = V   4 (-5.8) 4
             , up          = V   5  6.2  4
             , focalLength = 7
             , width       = squareSide
             , height      = squareSide
             , cellLength  = 6.8 / fromIntegral squareSide
             }

      -- image1 bunnyMesh =
      --   let (len, rng) = (5, [1..len])
      --       bunnyScalar = 7
      --       bunnyObjs = flip map [V x 0 z | x <- rng, z <- rng] $ \p ->
      --                      Obj (Right bunnyMesh) (color p) (xform p)
      --       color (V x _ z) = matFromColor (RGB $ V (x / len) (z / len) 0.5)
      --       xform v@(V x _ z) = mkAffine [translateA v, rotateA Y deg, scaleA bunnyScalar]
      --         where deg = negate $ 10 * (len-z) + 5 * x

      --       objs' = octreeFromObjects (floorObj:bunnyObjs)
      --       scene' = scene{objs = objs'}

      --   in mkImage "persp" scene' cam2 perspectiveRayGen

      image1 _ = mkImage "persp" scene' cam orthographicRayGen
        where scene' = scene{objs = floorObj:spheres}


-- With all the info, make the image and save it to disk
mkImage :: Intersector i => FilePath -> Scene i -> Camera -> RayGenFunc -> IO ()
mkImage !fp !scene !cam !rgf =
    let pixelFunc !x !y = toRGBPixel $ raycast scene cam rgf x y
        outPath   = dataDir ++ "/" ++ fp ++ ".png"

    in ImageWriter.writeImage outPath pixelFunc (width cam) (height cam)
