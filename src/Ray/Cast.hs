{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Ray.Cast where

import           Ray.Affine
import           Ray.Geometry
import           Ray.Intersection
import           Ray.Mesh
import           Ray.Scene
import           Ray.Shape

import           Data.List    (minimumBy)
import           Data.Ord     (comparing)
import qualified Data.Maybe as Maybe

import qualified Control.Monad.Random.Class as Random
import qualified Control.Monad.Random as Random

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
        reflD  = reflectionDepth . colorDetails $ scene
        colors = map (computeColor reflD scene) rays
    in averageColors colors

-- Given a bunch of RGBPixels, take the mean value
averageColors :: [RGBPixel] -> RGBPixel
averageColors pixels = RGB . average $ map unRGB pixels

average :: (Fractional a, Num a) => [a] -> a
average [] = 0
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
                    Emissive      -> white    -- Don't bother with emissive materials
                    SolidColor{}  -> directLt
                    _             -> lighting


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
        (RGB diffuseColor, RGB specularColor) = getColorForObject untransPos eitherShapeMesh metaData material

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
        r = eyeDir `reflectedOver` normalDir
        (glossiness, kRays) = if isReflective material then (glossyFactor material, numRays material)
                                                       else (1.0, 0)
        reflDirs     = getReflectionDirections glossiness r normalDir pos kRays
        indirectRefl = unRGB $ averageColors $ over reflDirs $ \reflDir ->
                          computeColor (reflDepth-1) scene reflDir

        -- Since reflDepth is set to 0 on non-reflective materials
        -- we can just check if reflDepth > 0 to check for reflectivity
        reflectLt  = reflComponent material * (if reflDepth > 0 then indirectRefl else V 0 0 0)


        refrac = if isTransparent material then refractiveIndex material else 1
        transmitDir = computeTransmissionRay normalDir eyeDir pos refrac
        transmission = unRGB . computeColor (reflDepth - 1) scene <$> transmitDir
        hasTransmission = isTransparent material && reflDepth > 0
        mTransmitLt = if hasTransmission then transmission else (Just $ V 0 0 0)
        transmitLt = (transmitComponent material * grabEtaSqr refrac normalDir eyeDir) .* Maybe.fromMaybe (V 0 0 0) mTransmitLt
        -- green means not transparent or refl depth expired
        -- purple means total internal reflection

        indirectLt = reflectLt + transmitLt

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


getColorForObject :: V3 -> Either Shape Mesh -> IntersectionMetaData Object -> Material -> (RGBPixel, RGBPixel)
getColorForObject p o meta Texture{..} =
    let (u,v) = getObjectUV o p meta
    in (diffuseColorF u v, specularColorF u v)
getColorForObject _ _ _ m = getColorsFromMaterial m


-- Retrieves rays given the glossiness, reflection direction,
-- normal direction, intersection point, and the number of rays
getReflectionDirections :: Double -> V3 -> V3 -> V3 -> Int -> [Ray]
getReflectionDirections glossiness r@(V x y z) n pos kRays =
    let pi2 = pi / 2

        -- Random angle between 0 and pi / 2 biased toward 0 by glossiness
        phi :: Random.MonadRandom m => m Double
        phi =
            let phiFromA a = pi2 * (cos a ** glossiness)
            in phiFromA <$> Random.getRandomR (0, pi2)

        -- We're generating spherical points on unit sphere
        sphericalToCart p t =
            let sinp = sin p
            in V (sin t * sinp) (cos p) (sinp * cos t)

        r' = normalize r
        (u, w) = getUVBasisForNormal r'
        rotateSpherical (V a b c) = (a .* u) + (b .* r') + (c .* w)

        randDir :: Random.MonadRandom m => m V3
        randDir = do t <- Random.getRandomR (0, 2 * pi)
                     p <- phi
                     return . rotateSpherical $ sphericalToCart p t

        many :: Random.MonadRandom m => m a -> m [a]
        many m = do m'  <- m
                    ms <- many m
                    return (m':ms)

        takeKThat k m p = take k . filter p <$> m

        goodDirs :: Random.MonadRandom m => m [V3]
        goodDirs = takeKThat kRays (many randDir) $ \v -> v .*. n > 0

        rayPerturb v = ray (pos + v *. 1e-10) v

        rays = map rayPerturb <$> goodDirs

        seed = Random.mkStdGen (round $ x + y + z)

    in Random.evalRand rays seed

-- Gets the Transmission ray given the normal direction,
-- the view direction, the intersection point
-- and the refractive index. Returns Nothing if total internal reflection
-- #419begin
--   #type=1
--   #src=http://www.starkeffects.com/snells-law-vector.shtml
-- #419end
computeTransmissionRay :: V3 -> V3 -> V3 -> Double -> Maybe Ray
computeTransmissionRay n' w0 p e =
    let insideObj = n' .*. w0 < 0
        eta = if insideObj then e else 1.0 / e
        n   = if insideObj then negate n' else n'

        s1   = negate w0
        ns1  = n `cross` s1
        ns1' = negate ns1

        v1 = eta .* (n `cross` ns1')
        e2 = eta * eta
        k = ns1 .*. ns1
        c = 1 - e2 * k

        totalInternalReflection = c < 0

        tDir = v1 - (n *. sqrt c)

        t = Ray (p + 1e-10 .* tDir) tDir

        result = if totalInternalReflection then Nothing else Just t

    in result

grabEtaSqr :: Double -> V3 -> V3 -> Double
grabEtaSqr e n w0 =
    let eta = if n .*. w0 >= 0 then e else 1 / e
    in eta * eta
