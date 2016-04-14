{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Ray.Scene where

import           Ray.Affine
import           Ray.AreaLightShape
import           Ray.Geometry
import           Ray.Intersection
import           Ray.Mesh
import           Ray.Octree
import           Ray.Shape
import           Data.Word (Word8)

-- Simple triplet that contains Red, Green, and Blue components -- Each component will range between [0, 1]
newtype RGBPixel = RGB { unRGB :: V3 } deriving (Eq, Ord, Show)

toRGBPixel :: RGBPixel -> (Word8, Word8, Word8)
toRGBPixel (RGB (V r g b)) = (rip r, rip g, rip b)
  where rip x = clamp 0 255 (round (255.0 * x))
{-# INLINE toRGBPixel #-}

clamp :: Ord a => a -> a -> a -> a
clamp a b x = max a (min b x)
{-# INLINE clamp #-}

-- Smart constructor
rgb :: Double -> Double -> Double -> RGBPixel
rgb x y z = RGB $ V x' y' z'
  where clean = clamp 0 1
        (x', y', z') = (clean x, clean y, clean z)

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
white = rgb 1 1 1

-- Materials for objects
data Material = SolidColor {
                  diffuseColor  :: RGBPixel
                , specularColor :: RGBPixel
                }
              | Emissive -- always comes up as white
              deriving (Eq, Ord, Show)

-- Light sources
data Light = DirectionalLight RGBPixel V3 -- a color and a direction
           | PointLight RGBPixel V3 -- a color and a position
           | AreaLight RGBPixel Int AreaLightShape -- a color, num samples, and a region
           deriving (Show)

data ColorDetails = ColorDetails {
                      backgroundColor     :: RGBPixel
                    , ambientCoefficient  :: Double
                    , ambientIntensity    :: RGBPixel
                    , diffuseCoefficient  :: Double
                    , specularCoefficient :: Double
                    , specularExponent    :: Double
                    } deriving (Show)

-- Smart constructor
directionalLight :: Double -> RGBPixel -> V3 -> Light
directionalLight intensity color dir = DirectionalLight color' (normalize dir)
  where color' = RGB (intensity .* unRGB color)

-- Smart constructor
pointLight :: Double -> RGBPixel -> V3 -> Light
pointLight intensity color pos = PointLight color' pos
  where color' = RGB (intensity .* unRGB color)

-- Smart constructor
areaLight :: Double -> RGBPixel -> Int -> AreaLightShape -> Light
areaLight intensity color = AreaLight color'
  where color' = RGB (intensity .* unRGB color)


type InternalGeometry = Either Shape Mesh
-- Something in the scene with a given 3D geometry and material
data Object = Obj {
                geometry  :: InternalGeometry
              , material  :: Material
              , transform :: AffineBijection
              } deriving (Show)

instance IntersectObj Object where
  type IntersectionMetaData Object = Maybe Face
  getIntersect r (Obj (Left shape) _ tr) =
      let r' = invertRay tr r
      in case getIntersect r' shape of
             Nothing     -> Nothing
             Just (t, _) -> Just (t, Nothing)
  getIntersect r (Obj (Right mesh) _ tr) =
      let r' = invertRay tr r
      in case getIntersect r' mesh of
             Nothing     -> Nothing
             Just (t, f) -> Just (t, Just f)

instance AABB Object where
  type Dict Object = ()
  getBounds _ (Obj (Left shape) _ tr)       = alterBounds tr $ getShapeBounds shape
  getBounds _ (Obj (Right (Mesh _ o)) _ tr) = alterBounds tr $ getOctreeBounds o

alterBounds :: AffineBijection -> Bounds -> Bounds
alterBounds ab (Bounds (V nx ny nz) (V mx my mz)) =
    let corners = [ V nx ny nz
                  , V mx ny nz
                  , V nx ny mz
                  , V mx ny mz
                  , V mx my mz
                  , V nx my mz
                  , V mx my nz
                  , V nx my nz
                  ]

        projected = map (projectPoint ab) corners

        minmax (V nx' ny' nz', V mx' my' mz') (V x y z) =
            (V (min nx' x) (min ny' y) (min nz' z),
             V (max mx' x) (max my' y) (max mz' z))

        (minV, maxV) = foldl minmax (head projected, head projected) projected

     in Bounds minV maxV

-- A scene has a few objects, a list of light sources, and a background color
data Scene container = Scene {
                         objs         :: container Object
                       , lights       :: [Light]
                       , colorDetails :: ColorDetails
                       }


-- Given an Object, a point of intersection, and intersection metadata,
-- calculate the normal direction f
getUnnormalizedObjectNormal :: InternalGeometry -> V3 -> IntersectionMetaData Object -> V3
getUnnormalizedObjectNormal (Left  shape)        pos _           = computeNormal shape pos
getUnnormalizedObjectNormal (Right _    )        _   Nothing     = V 1 0 0 -- Impossible case
getUnnormalizedObjectNormal (Right (Mesh vs _ )) _   (Just face) = getFaceNormal vs face


-- Convenience function to rip out diffuse and specular colors
getColorsFromMaterial :: Material -> (RGBPixel, RGBPixel)
getColorsFromMaterial SolidColor{..} = (diffuseColor, specularColor)
getColorsFromMaterial Emissive       = (white, white)

-- Gets unnormalized Rays from a Light and a target point
-- where the Ray origin is used for calculating visibility
-- and the magnitude is the "length" of the light path
getLightRays :: Light -> V3 -> [(Double, Ray)]
getLightRays (DirectionalLight _ dir) p = [(1e10, Ray p (negate dir))]
getLightRays (PointLight       _ pos) p = [(dist-1e-9, Ray p (d /. dist))]
  where d    = pos - p
        dist = magnitude d
getLightRays (AreaLight       _ k sh) p =
    let rayOrigins = getSamplePoints k sh
    in flip map rayOrigins $ \pos ->
         let d    = pos - p
             dist = magnitude d
         in (dist-1e-9, Ray p (d /. dist))


getLightIntensity :: Light -> RGBPixel
getLightIntensity (DirectionalLight i _  ) = i
getLightIntensity (PointLight       i _  ) = i
getLightIntensity (AreaLight        i _ _) = i

-- Given a Light and a point of intersection, what is the squared radius of distance?
lightDistanceSquared :: Light -> V3 -> Double
lightDistanceSquared (DirectionalLight _ _      ) _ = 1 -- so that there is no attenuation
lightDistanceSquared (PointLight       _ pos    ) p = let v = (pos - p) in v .*. v
lightDistanceSquared (AreaLight        _ _ shape) p = areaDistanceSquared shape p


octreeFromObjects :: [Object] -> Octree Object
octreeFromObjects = octreeFromList ()
