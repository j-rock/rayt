{-# LANGUAGE RecordWildCards #-}
module Ray.Cast where

import           Ray.Geometry

import           Data.List    (minimumBy)
import           Data.Maybe   (mapMaybe)
import           Data.Ord     (comparing)

import qualified Data.Vector  as V

-- Simple triplet that contains Red, Green, and Blue components
-- Each component will range between [0, 1]
data RGBPixel = RGB V3 deriving (Eq, Ord, Show)

toRGBPixel :: Integral a => RGBPixel -> (a, a, a)
toRGBPixel (RGB (V r g b)) = (rip r, rip g, rip b)
  where rip x = clamp 0 255 (round (255.0 * x))

clamp :: Ord a => a -> a -> a -> a
clamp a b x = max a (min b x)

-- Smart constructor
rgb :: Double -> Double -> Double -> RGBPixel
rgb x y z = RGB (normalize $ V x y z)

red :: RGBPixel
red = rgb 1 0 0

yellow :: RGBPixel
yellow = rgb 1 1 0

green :: RGBPixel
green = rgb 0 1 0

blue :: RGBPixel
blue = rgb 0 0 1

black :: RGBPixel
black = rgb 0 0 0

white :: RGBPixel
white = rgb 0 0 0

-- Materials for objects (right now just solid colors)
data Material = SolidColor RGBPixel
              | CheckerBoard RGBPixel RGBPixel
              deriving (Eq, Ord, Show)

-- Light sources
-- (So far we only support directional lights)
data Light = DirectionalLight Double V3 -- a color intensity and a direction
           deriving (Eq, Ord, Show)

-- Smart constructor
directionalLight :: Double -> V3 -> Light
directionalLight intensity dir = DirectionalLight intensity (normalize dir)

-- Something in the scene with a given 3D geometry and material
data Object = Obj Shape Material deriving (Eq, Ord, Show)

-- A scene has a few objects, a list of light sources, and a background color
data Scene = Scene [Object] [Light] RGBPixel deriving (Eq, Ord, Show)

data Camera = Camera {
                position    :: V3
              , lookAt      :: V3
              , up          :: V3
              , focalLength :: Double
              , width       :: Int
              , height      :: Int
              , cellLength  :: Double
              } deriving (Eq, Ord, Show)

-- Given view plane grid coordinates and a camera, create some rays.
-- Single-ray casts will return a singleton list.
-- Multi-jittered casts will return a few rays in the list.
type RayGenFunc = (Int, Int) -> Camera -> [Ray]

-- Given a scene, a camera, a way to produce rays from a camera and grid cell,
-- generate the RGBPixels that represent the captured scene.
-- The return type is a function that takes two image coordinates and returns a pixel.
-- This way we can hide the underlying Vector of Vectors
raycast :: Scene -> Camera -> RayGenFunc -> (Int -> Int -> RGBPixel)
raycast scene cam@Camera{..} rgf =
    let vec = V.generate width $ \x ->
                  V.generate height $ \y ->
                      let rays   = rgf (x, y) cam
                          colors = map (computeColor scene) rays
                      in averageColors colors
    in \x y -> (vec V.! x) V.! y


-- Given a Scene with objects and lights, and a ray, compute the
-- color such a Ray would get
computeColor :: Scene -> Ray -> RGBPixel
computeColor scene@(Scene objs _ bgc) r@(Ray rayOrigin rayDir) =
    let getIntersect :: Object -> Maybe (Double, Object)
        getIntersect o@(Obj s _) =
            case intersectWithRay s r of
                Nothing -> Nothing
                Just t'  -> Just (t', o)

        -- These objects intersected the ray.
        -- It is a list of pairs of where on the ray there was an intersection
        -- and which object intersected it
        intersects :: [(Double, Object)]
        intersects = mapMaybe getIntersect objs

        -- The position along the ray and object of the closest intersection
        (t, obj) = minimumBy (comparing fst) intersects

        intersectPosition = rayOrigin + (t .* rayDir)

        lighting = computeLighting scene r obj intersectPosition

    in if null intersects
           then bgc -- No intersections, so return background color
           else lighting


-- Given a bunch of RGBPixels, take the mean value
averageColors :: [RGBPixel] -> RGBPixel
averageColors pixels = RGB (theSum / numPixels)
  where theSum    = sum $ map (\(RGB v) -> v) pixels
        numPixels = fromIntegral (length pixels)


-- Uses lights and material to compute the pixel color.
-- Takes a scene, the originating ray, the intersected object,
-- and the point of ray-object intersection.
computeLighting :: Scene -> Ray -> Object -> V3 -> RGBPixel
computeLighting (Scene _ lights _) _ obj@(Obj shape _) pos =
    let objNormal :: V3
        objNormal = computeNormal shape pos

        lambertianIntensity = sum $ map (lambertian objNormal) lights

        RGB objColor = getObjectColor obj pos

        rawIntensity = lambertianIntensity
        finalIntensity = clamp 0 1 rawIntensity


    in RGB (finalIntensity .* objColor)


-- Computes the amount of diffuse light given a normal direction and a light.
--Taken from Wikipedia - Lambertian Reflectance
lambertian :: V3 -> Light -> Double
lambertian normal (DirectionalLight lightI dir) = max 0 (dir .*. normal) * lightI


-- Allows for fancier textures as we go along
getObjectColor :: Object -> V3 -> RGBPixel
getObjectColor (Obj _ (SolidColor color)) _ = color
getObjectColor (Obj s (CheckerBoard a b)) v = getCheckerboardColor s a b v

-- Chooses between two colors for a given Shape and a point on that shape
getCheckerboardColor :: Shape -> RGBPixel -> RGBPixel -> V3 -> RGBPixel
getCheckerboardColor (Plane normal d) a b v =
    let V nx ny _  = normal
        p1 = V (-d / nx) 0 0
        p2 = V (-d / ny) 0 0
    in choose (magnitude $ v - p1) (magnitude $ v - p2) a b

getCheckerboardColor s@(Sphere c r) a b v =
    let V x y z = computeNormal s v
    in choose (acos z / r) (atan $ y / x) a b

getCheckerboardColor tri@(Triangle v _ _) a b v' = getCheckerboardColor trianglePlane a b v'
    where normal = computeNormal tri v
          trianglePlane = planeFromNormalAndPoint normal v


-- Chooses a color given two values
choose :: Double -> Double -> RGBPixel -> RGBPixel -> RGBPixel
choose x y a b = if c * c >= 0.5 then a else b
  where c = cos (x + y)
