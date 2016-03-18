{-# LANGUAGE TypeFamilies #-}

module Ray.Intersection where

import Ray.Geometry

import qualified Data.Maybe as Maybe

-- So we have two main typeclasses here:
--   IntersectObj: something that can be intersected with a ray and
--       provide metadata on the intersection
--       This enables us to combine boring shapes and meshes in the same scene.
--       Such that I can make efficient intersections on both.
--
--   Intersector: A container for IntersectObjs, effectively.
--      This way I can switch out octree's and singly-linked lists
--      for performance comparisons.
--
--
-- There is not much reason to look at the code here... it's really boilerplatey.

class IntersectObj a where
  type IntersectionMetaData a :: *
  getIntersect        :: Ray -> a -> Maybe (Double, IntersectionMetaData a)
  intersectObjWithRay :: Ray -> a -> Maybe (Double, a, IntersectionMetaData a)
  intersectObjWithRay r a =
      case getIntersect r a of
          Nothing -> Nothing
          Just (t, m)  -> Just (t, a, m)


class Intersector f where
  getIntersects :: IntersectObj a => f a -> Ray -> [(Double, a, IntersectionMetaData a)]

instance Intersector [] where
  getIntersects objs r = Maybe.mapMaybe (intersectObjWithRay r) objs
