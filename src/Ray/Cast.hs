{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Ray.Cast where

import           Ray.Geometry
import           Ray.Intersection
import           Ray.Scene


import           Data.List    (minimumBy)
import           Data.Ord     (comparing)

import Debug.Trace

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
raycast :: Intersector i =>  Scene i -> Camera -> RayGenFunc -> (Int -> Int -> RGBPixel)
raycast scene cam@Camera{..} rgf x y =
    let rays   = rgf (x, y) cam
        colors = map (computeColor scene) rays
    in averageColors colors

-- Given a bunch of RGBPixels, take the mean value
averageColors :: [RGBPixel] -> RGBPixel
averageColors pixels = RGB (theSum / numPixels)
  where theSum    = sum $ map (\(RGB v) -> v) pixels
        numPixels = fromIntegral (length pixels)

-- Given a Scene with objects and lights, and a ray, compute the
-- color such a Ray would get
computeColor :: Intersector i => Scene i -> Ray -> RGBPixel
computeColor scene r@(Ray rayOrigin rayDir) =
    let -- These objects intersected the ray.
        -- It is a list of pairs of where on the ray there was an intersection
        -- and which object intersected it
        intersects :: [(Double, Object, IntersectionMetaData Object)]
        intersects = getIntersects (objs scene) r

        -- The position along the ray and object of the closest intersection
        (t, obj, m) = minimumBy (comparing (\(a,_,_) -> a)) intersects

        intersectPosition = rayOrigin + (t .* rayDir)

        lighting = computeLighting scene r intersectPosition obj m

    in if null intersects
           then backgroundColor $ colorDetails scene -- No intersections, so return background color
           else lighting


-- Uses lights and material to compute the pixel color.
-- Takes a scene, the originating ray, the point of intersection,
-- the object intersected, and any intersection metadata.
computeLighting :: Intersector i => Scene i -> Ray -> V3 -> Object -> IntersectionMetaData Object -> RGBPixel
computeLighting scene viewRay pos (Obj eitherShapeMesh material) metaData =
    let -- the direction from the point of intersection to the camera
        eyeDir    = negate $ getRayDir viewRay

        -- the normal direction at the point of intersection
        normalDir = getObjectNormal eitherShapeMesh pos metaData

        -- The colors of the intersected object
        (RGB diffuseColor, RGB specularColor) = getColorsFromMaterial material

        -- Keep only those lights unobstructed by objects
        visibleLights = filter (isVisible pos $ objs scene) (lights scene)

        colorDs       = colorDetails scene
        diffuseCoeff  = diffuseCoefficient colorDs
        specularCoeff = specularCoefficient colorDs
        specularExp   = diffuseCoefficient colorDs

        -- Sum together all the remaining lights
        rawColor = sum $ over visibleLights $ \light ->
                          let -- The direction from the intersection point to the light
                              lightDir     = getLightDir light pos
                              reflectedDir = lightDir `reflectedOver` normalDir
                              cosThetaL    = max 0 $ normalDir .*. lightDir

                              lightIntensity = cosThetaL * getLightIntensity light

                              diffuseComponent  = (diffuseCoeff / pi) .* diffuseColor
                              specularComponent = let dotProd = max 0 $ reflectedDir .*. eyeDir
                                                      powered = dotProd ** specularExp
                                                      specd   = specularCoeff * powered
                                                  in specd .* specularColor

                              lightComponent = lightIntensity / lightDistanceSquared light pos

                              result = lightComponent .* (diffuseComponent + specularComponent)

                          in result

        clampedColor = clampV (V 0 0 0) (V 1 1 1) rawColor
        out = RGB clampedColor

    in out

over :: [a] -> (a -> b) -> [b]
over = flip map

-- Given a point in space, and all the objects in a scene, can a given
-- Light actually illuminate that point in space?
isVisible :: Intersector i => V3 -> i Object -> Light -> Bool
isVisible p objs (DirectionalLight _ dir) = isRayUnobstructedByT p (negate dir) 1e10 objs
isVisible p objs (PointLight       _ pos) = isRayUnobstructedByT pos lightDir t objs
  where objectToLightDir = pos - p
        distance = magnitude objectToLightDir
        lightDir = objectToLightDir *. (1 / distance)
        t        = distance - 1e-9

-- Takes a starting position, a direction, and finds if there are any collisions
-- in the Scene up until a certain point (a distance along the ray)
isRayUnobstructedByT :: Intersector i => V3 -> V3 -> Double -> i Object -> Bool
isRayUnobstructedByT start dir t objs =
    let r = Ray (start + 1e-10 .* dir) dir
        intersects = getIntersects objs r
    in all (\(t',_,_) -> t' > t) intersects
