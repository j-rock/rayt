{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Ray.Cast where

import           Ray.Affine
import           Ray.Geometry
import           Ray.Intersection
import           Ray.Scene

import           Data.List    (minimumBy)
import           Data.Ord     (comparing)

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
        reflD  = mirrorReflectionDepth . colorDetails $ scene
        colors = map (computeColor reflD scene) rays
    in averageColors colors

-- Given a bunch of RGBPixels, take the mean value
averageColors :: [RGBPixel] -> RGBPixel
averageColors pixels = RGB . average $ map unRGB pixels

average :: (Fractional a, Num a) => [a] -> a
average xs = sum xs / fromIntegral (length xs)

-- Given a Scene with objects and lights, a ray,
-- and a mirror reflection depth, compute the
-- color such a Ray would get
computeColor :: Intersector i => Int -> Scene i -> Ray -> RGBPixel
computeColor reflDepth scene r@(Ray rayOrigin rayDir) =
    let -- These objects intersected the ray.
        -- It is a list of pairs of where on the ray there was an intersection
        -- and which object intersected it
        intersects :: [(Double, Object, IntersectionMetaData Object)]
        intersects = getIntersects (objs scene) r

        -- The position along the ray and object of the closest intersection
        (t, obj, m) = minimumBy (comparing (\(a,_,_) -> a)) intersects

        Ray virtualOrigin virtualDir = invertRay (transform obj) r
        intersectPosition = rayOrigin + (t .* rayDir)
        virtualPosition   = virtualOrigin + (t .* virtualDir)

        lighting = computeLighting reflDepth scene r intersectPosition virtualPosition obj m
        directLt = computeLighting 0         scene r intersectPosition virtualPosition obj m

    in if null intersects
           then backgroundColor $ colorDetails scene -- No intersections, so return background color
           else case material obj of
                    Emissive     -> white    -- Don't bother with emissive materials
                    SolidColor{} -> directLt
                    _            -> lighting


-- Uses lights and material to compute the pixel color.
-- Takes a scene, the originating ray, the point of intersection,
-- the object intersected, any intersection metadata, and the
-- mirror reflection depth
computeLighting :: Intersector i =>
                   Int -> Scene i -> Ray ->
                   V3 -> V3 -> Object ->
                   IntersectionMetaData Object -> RGBPixel
computeLighting reflDepth scene viewRay pos untransPos (Obj eitherShapeMesh material tr) metaData =
    let -- the direction from the point of intersection to the camera
        eyeDir    = negate $ getRayDir viewRay

        -- the normal direction at the point of intersection on the untransformed object
        untransNormal = getUnnormalizedObjectNormal eitherShapeMesh untransPos metaData
        normalDir     = normalize $ invertNormal tr untransNormal


        -- The colors of the intersected object
        (RGB diffuseColor, RGB specularColor) = getColorsFromMaterial material

        colorDs       = colorDetails scene
        ambientCoeff  = ambientCoefficient colorDs
        ambientIntens = unRGB $ ambientIntensity colorDs
        diffuseCoeff  = diffuseCoefficient colorDs
        specularCoeff = specularCoefficient colorDs
        specularExp   = diffuseCoefficient colorDs

        -- Sum together all the remaining lights
        nonAmbDirect = sum $ over (lights scene) $ \light ->
                          let values = over (getLightRays light pos) $ \(t, lightRay) ->
                                           let lightDir       = getRayDir lightRay
                                               reflectedDir   = lightDir `reflectedOver` normalDir
                                               cosThetaL      = max 0 $ normalDir .*. lightDir

                                               lightIntensity = cosThetaL .* (unRGB . getLightIntensity) light

                                               diffuseComponent  = (diffuseCoeff / pi) .* diffuseColor
                                               specularComponent = let dotProd = max 0 $ reflectedDir .*. eyeDir
                                                                       powered = dotProd ** specularExp
                                                                       specd   = specularCoeff * powered
                                                                   in specd .* specularColor

                                               lightComponent = lightIntensity /. lightDistanceSquared light pos

                                               result = lightComponent * (diffuseComponent + specularComponent)

                                               obstructed = not $ isVisibleLightRay t lightRay $ objs scene

                                           in if obstructed
                                              then V 0 0 0 -- no light contribution from this ray
                                              else result

                          in average values

        ambientLightComp = ambientCoeff .* ambientIntens
        directLt = ambientLightComp + nonAmbDirect

       -- Now to compute indirect lighting
        reflDir     = eyeDir `reflectedOver` normalDir
        indirectRec = unRGB $ computeColor (reflDepth-1) scene (ray pos' reflDir)
          where pos' = pos + 1e-10 .* reflDir
        indirectLt  = reflComponent material * (if reflDepth > 0 then indirectRec else V 0 0 0)


        unclamped = directLt + indirectLt

        clampedColor = clampV (V 0 0 0) (V 1 1 1) unclamped
        out = RGB clampedColor

    in out

over :: [a] -> (a -> b) -> [b]
over = flip map


isVisibleLightRay :: Intersector i => Double -> Ray -> i Object -> Bool
isVisibleLightRay t (Ray start dir) = isRayUnobstructedByT start dir t

-- Takes a starting position, a direction, and finds if there are any collisions
-- in the Scene up until a certain point (a distance along the ray)
isRayUnobstructedByT :: Intersector i => V3 -> V3 -> Double -> i Object -> Bool
isRayUnobstructedByT start dir t objs =
    let r = Ray (start + 1e-10 .* dir) dir
        intersects = getIntersects objs r
    in all (\(t',_,_) -> t' > t) intersects
