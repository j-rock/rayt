{-# LANGUAGE BangPatterns #-}

module Main where

import qualified ImageWriter
import           Control.Monad    (when)
import           Ray
import qualified Data.Maybe as Maybe
import qualified System.Directory as Dir

-- This is the directory where the output images will be (re)placed
dataDir :: FilePath
dataDir = "./rayt-imgs"

-- Entry point to program
main :: IO ()
main = do setupDataDirectory

          say "Generating scene"
          image1

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

      -- floorC   = RGB $ V 0.449 0.597 0.84375
      floorC   = RGB $ V 0.1 0.1 0.1
      floorMat = Reflective floorC floorC white 0.3
      greenM   = SolidColor (RGB $ V 1 0.4 0) (RGB $ V 0.1 0.1 0)
      redMat   = Reflective red red white 0.3

      -- emissive = Emissive

      makeShapeObj = Obj . Left

      floorPlane = planeFromNormalAndPoint (V 0 1 0) (V 0 0 0)
      floorObj = makeShapeObj floorPlane floorMat noAffineTrans

      sphereSh = Sphere (V 0 0 0) 1
      sphere1 = makeShapeObj sphereSh redMat $ mkAffine [translateA (V 0 1.4 0)]
      sphere2 = makeShapeObj sphereSh greenM $ mkAffine [translateA (V 3 3 2), rotateA X 15, scaleA (V 0.3 0.3 0.2)]


      objects = [
                  floorObj
                , sphere1
                , sphere2
                ]


      light1 = directionalLight 0.2 white $ V 0 (-1) 0
      light2 = directionalLight 0.7 white $ V (-1) 0 0
      light3 = directionalLight 0.7 white $ V 1 0 0
      light4 = pointLight 1.0 white $ V 0 0.1 0
      light5 = areaLight 1.0 white 20 $ Rect3D (V 1.4 1 (-0.7)) (V 0 0 0.1) (V 0 0.1 0)
      light6 = areaLight 4.0 white 20 $ Rect3D (V 0.7 1 3.4) (V (-1) 0 0) (V 0 1 0)
      light7 = areaLight 10.0 white 20 $ Rect3D (V 1.4 5 (-0.7)) (V 0 0 1) (V 1 0 0)
      lts    = [
               --   light1
               -- , light2
               -- , light3
                 light5
               , light6
               , light7
               ]

      colorDeets = ColorDetails {
                     backgroundColor       = RGB (V 0.5 0.5 0.5)
                   , ambientCoefficient    = 0.009
                   , ambientIntensity      = white -- RGB (V 1 0.7 0.7)
                   , diffuseCoefficient    = 0.8
                   , specularCoefficient   = 0.2
                   , specularExponent      = 20
                   , mirrorReflectionDepth = 1
                   }


      scene = mkScene $ Scene objects lts colorDeets


      squareSide = 400
      camPos   = V 3 3 (-3)
      lookinAt = V 1.6 1.3 0
      maxWidth = 5
      upV      = let V x y  z = lookAt cam
                     y'       = negate $ (x*x + z*z) / y
                 in  normalize $ V x y' z
      cam = Camera {
              position    = camPos
            , lookAt      = normalize $ lookinAt - camPos
            , up          = upV
            , focalLength = 1
            , width       = squareSide
            , height      = squareSide
            , cellLength  = maxWidth / fromIntegral squareSide
            }

      image1 = mkImage "scene" scene cam orthographicRayGen


-- With all the info, make the image and save it to disk
mkImage :: Intersector i => FilePath -> Scene i -> Camera -> RayGenFunc -> IO ()
mkImage !fp !scene !cam !rgf =
    let pixelFunc !x !y = toRGBPixel $ raycast scene cam rgf x y
        outPath   = dataDir ++ "/" ++ fp ++ ".png"

    in ImageWriter.writeImage outPath pixelFunc (width cam) (height cam)

mkScene :: Scene [] -> Scene []
mkScene s =
    let lightToGeometry :: Light -> Maybe Object
        lightToGeometry (AreaLight _ _ shape) = Just $ Obj geom Emissive noAffineTrans
          where geom = computeShapeOrMesh shape
        lightToGeometry _ = Nothing

        lightObjs = Maybe.mapMaybe lightToGeometry $ lights s

    in s{objs = lightObjs ++ objs s}
