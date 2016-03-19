{-# LANGUAGE TypeFamilies #-}

module Ray.Scene where

import           Ray.Geometry
import           Ray.Intersection
import           Ray.Mesh
import           Ray.Octree
import           Ray.Shape

-- Simple triplet that contains Red, Green, and Blue components -- Each component will range between [0, 1]
data RGBPixel = RGB V3 deriving (Eq, Ord, Show)

toRGBPixel :: Integral a => RGBPixel -> (a, a, a)
toRGBPixel (RGB (V r g b)) = (rip r, rip g, rip b)
  where rip x = clamp 0 255 (round (255.0 * x))

clamp :: Ord a => a -> a -> a -> a
clamp a b x = max a (min b x)

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
                    , diffuseCoefficient  :: Double
                    , specularCoefficient :: Double
                    , specularExponent    :: Double
                    } deriving (Show)

-- Smart constructor
directionalLight :: Double -> V3 -> Light
directionalLight intensity dir = DirectionalLight intensity (normalize dir)

type InternalGeometry = Either Shape Mesh
-- Something in the scene with a given 3D geometry and material
data Object = Obj InternalGeometry Material deriving (Show)

instance IntersectObj Object where
  type IntersectionMetaData Object = Maybe Face
  getIntersect r (Obj (Left shape) _) = case getIntersect r shape of
                                          Nothing     -> Nothing
                                          Just (t, _) -> Just (t, Nothing)
  getIntersect r (Obj (Right mesh) _) = case getIntersect r mesh of
                                          Nothing     -> Nothing
                                          Just (t, f) -> Just (t, Just f)

instance AABB Object where
  type Dict Object = ()
  getBounds _ (Obj (Left shape) _)       = getShapeBounds shape
  getBounds _ (Obj (Right (Mesh _ o)) _) = getOctreeBounds o -- getOctreeBounds o

-- A scene has a few objects, a list of light sources, and a background color
data Scene container = Scene {
                         objs         :: container Object
                       , lights       :: [Light]
                       , colorDetails :: ColorDetails
                       }


-- Given an Object, a point of intersection, and intersection metadata,
-- calculate the normal direction f
getObjectNormal :: InternalGeometry -> V3 -> IntersectionMetaData Object -> V3
getObjectNormal (Left  shape)        pos _           = computeNormal shape pos
getObjectNormal (Right _    )        _   Nothing     = V 1 0 0 -- Impossible case
getObjectNormal (Right (Mesh vs _ )) _   (Just face) = getFaceNormal vs face


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
