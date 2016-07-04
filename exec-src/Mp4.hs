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
      floorMat = Reflective white white white 1.0 100.0 10

      mat1 = SolidColor white white
      mat2 = SolidColor white white
      mat3 = SolidColor white white
      husk = SolidColor black black
      orn  = SolidColor r r
        where r = RGB $ V 1.0 0.25 0.13671875


      makeShapeObj = Obj . Left

      floorPlane = planeFromNormalAndPoint (V 0 1 0) (V 0 0 0)
      floorObj = makeShapeObj floorPlane floorMat noAffineTrans

      sphereSh = Sphere (V 0 0 0) 1
      sphere1 = makeShapeObj sphereSh mat1 $ mkAffine [ translateA (V 0 0.5      0)]
      sphere2 = makeShapeObj sphereSh mat2 $ mkAffine [ translateA (V 0 1.833333 0)
                                                      , scaleA (2.01 / 3.0)
                                                      ]
      sphere3 = makeShapeObj sphereSh mat3 $ mkAffine [ translateA (V 0 2.91666 0)
                                                      , scaleA 0.51
                                                      ]
      sphere4 = makeShapeObj sphereSh husk $ mkAffine [ translateA (V (-0.2) 3.11666 0.45)
                                                      , scaleA 0.05
                                                      ]
      sphere5 = makeShapeObj sphereSh husk $ mkAffine [ translateA (V (0.2) 3.11666 0.45)
                                                      , scaleA 0.05
                                                      ]
      sphere6 = makeShapeObj sphereSh orn $ mkAffine [ translateA (V 0 3.11666 0.45)
                                                      , scaleA $ V 0.05 0.05 0.8
                                                      ]

      objects = [
                  floorObj
                , sphere1
                , sphere2
                , sphere3
                , sphere4
                , sphere5
                , sphere6
                ]

      light1 = directionalLight 3.0 white (V 0 (-0.5) (-1.0))
      light7 = areaLight 20.0 white 10 $ Rect3D (V 1.4 5 (-0.7)) (V 0 0 1) (V 1 0 0)
      light8 = areaLight 20.0 white 10 $ Rect3D (V 1 1.1 3) (V 1 0 0) (V 0 1 0)
      lts    = [
                 light1
               -- , light8
               ]

      colorDeets = ColorDetails {
                     backgroundColor       = RGB (V 0.6 0.2 0.9)
                   , ambientCoefficient    = 0.009
                   , ambientIntensity      = white
                   , diffuseCoefficient    = 0.8
                   , specularCoefficient   = 0.2
                   , specularExponent      = 6
                   , reflectionDepth       = 1
                   }


      scene = mkScene $ Scene objects lts colorDeets


      squareSide = 500
      camPos   = V 1.001 6.4 5
      lookinAt = V 0 1.0 0
      maxWidth = 2
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

      image1 = mkImage "scene" scene cam (multiJitteredRayGen 3)


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
