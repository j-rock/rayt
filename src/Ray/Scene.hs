{-# LANGUAGE TypeFamilies #-}

module Ray.Scene where

import           Ray.Affine
import           Ray.Geometry
import           Ray.Intersection
import           Ray.Mesh
import           Ray.Octree
import           Ray.Shape
import           Data.Word (Word8)

-- Simple triplet that contains Red, Green, and Blue components -- Each component will range between [0, 1]
data RGBPixel = RGB V3 deriving (Eq, Ord, Show)

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

-- Materials for objects (right now just solid colors)
data Material = SolidColor {
                  diffuseColor  :: RGBPixel
                , specularColor :: RGBPixel
                }
              deriving (Eq, Ord, Show)

-- Light sources
data Light = DirectionalLight Double V3 -- an intensity and a direction
           | PointLight Double V3 -- an intensity and a position
           deriving (Eq, Ord, Show)

data ColorDetails = ColorDetails {
                      backgroundColor     :: RGBPixel
                    , ambientCoefficient  :: Double
                    , ambientIntensity    :: Double
                    , diffuseCoefficient  :: Double
                    , specularCoefficient :: Double
                    , specularExponent    :: Double
                    } deriving (Show)

-- Smart constructor
directionalLight :: Double -> V3 -> Light
directionalLight intensity dir = DirectionalLight intensity (normalize dir)

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
getColorsFromMaterial m = (diffuseColor m, specularColor m)

-- Retrieves the direction of Light given the point of intersection
getLightDir :: Light -> V3 -> V3
getLightDir (DirectionalLight _ dir) _ = negate dir -- DirectionalLight is constant
getLightDir (PointLight       _ pos) p = normalize $ pos - p

getLightIntensity :: Light -> Double
getLightIntensity (DirectionalLight i _) = i
getLightIntensity (PointLight       i _) = i

-- Given a Light and a point of intersection, what is the squared radius of distance?
lightDistanceSquared :: Light -> V3 -> Double
lightDistanceSquared (DirectionalLight _ _  ) _ = 1 -- so that there is no attenuation
lightDistanceSquared (PointLight       _ pos) p = let v = (pos - p) in v .*. v


octreeFromObjects :: [Object] -> Octree Object
octreeFromObjects = octreeFromList ()
