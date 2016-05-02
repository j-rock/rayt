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

      checker c1 c2 u v =
          let k = 15
              m = cos (k * u) + cos (k * v)
          in if m > 0 then c1 else c2

      floorC   = RGB $ V 0.449 0.597 0.84375 *. 1.5
      floorMat = Texture (checker black white) (checker floorC white)
      mat1     = Transparent {
                   diffuseColor      = black
                 , specularColor     = white
                 , refractiveIndex   = 1.52
                 , transmissionCoeff = 0.8
                 }
      mat2     = Texture (checker black red) (checker red black)
      mat3     = Reflective{
                   diffuseColor  = black
                 , specularColor = white
                 , reflectColor  = white
                 , reflectCoeff  = 0.3
                 , glossyFactor  = 15.5
                 , numRays       = 40
                 }

      makeShapeObj = Obj . Left

      floorPlane = planeFromNormalAndPoint (V 0 1 0) (V 0 0 0)
      floorObj = makeShapeObj floorPlane floorMat noAffineTrans

      sphereSh = Sphere (V 0 0 0) 0.95
      sphere1 = makeShapeObj sphereSh mat1 $ mkAffine [translateA (V 2 1.4 0)]
      sphere2 = makeShapeObj sphereSh mat2 $ mkAffine [translateA (V 0 1.4 0)]
      sphere3 = makeShapeObj sphereSh mat3 $ mkAffine [translateA (V 1 1.4 (-2))]

      objects = [
                  floorObj
                , sphere1
                , sphere2
                , sphere3
                ]

      light7 = areaLight 20.0 white 10 $ Rect3D (V 1.4 5 (-0.7)) (V 0 0 1) (V 1 0 0)
      light8 = areaLight 20.0 white 10 $ Rect3D (V 1 1.1 3) (V 1 0 0) (V 0 1 0)
      lts    = [
                 light7
               , light8
               ]

      colorDeets = ColorDetails {
                     backgroundColor       = RGB (V 0.6 0.2 0.9)
                   , ambientCoefficient    = 0.009
                   , ambientIntensity      = white
                   , diffuseCoefficient    = 0.8
                   , specularCoefficient   = 0.2
                   , specularExponent      = 6
                   , reflectionDepth       = 6
                   }


      scene = mkScene $ Scene objects lts colorDeets


      squareSide = 500
      camPos   = V 5 3.4 (-5)
      lookinAt = V 1 1.4 0
      maxWidth = 5
      upV      = let V x y  z = lookAt cam
                     y'       = negate $ (x*x + z*z) / y
                 in  normalize $ V x y' z
      cam = Camera {
              position    = camPos
            , lookAt      = normalize $ lookinAt - camPos
            , up          = upV
            , focalLength = 2.2
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
