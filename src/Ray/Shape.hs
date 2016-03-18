{-# LANGUAGE TypeFamilies #-}

module Ray.Shape where

import           Ray.Geometry
import Ray.Intersection

data Shape = Plane V3 Double   -- A normal direction and a coefficient
           | Sphere V3 Double  -- A center point and a radius
           | Triangle V3 V3 V3 -- Three points in space (right-hand orientation)
           deriving (Show)

instance IntersectObj Shape where
  type IntersectionMetaData Shape = ()
  getIntersect = flip intersectWithRay

planeFromNormalAndPoint :: V3 -> V3 -> Shape
planeFromNormalAndPoint n p = Plane n' (negate $ n' .*. p)
  where n' = normalize n

-- A shape must be able to decide where it is intersected by a Ray
-- If successful, returns the distance along the ray where the intersection occurs
intersectWithRay :: Shape -> Ray -> Maybe (Double, ())

-- Algorithm ripped from Lecture-2
intersectWithRay (Plane n d) (Ray p v) =
    let vDotN = v .*. n
        t     = negate (p .*. n + d) / vDotN
    in if isZero vDotN || t <= 0
          then Nothing
          else Just (t, ())

intersectWithRay (Sphere sc r) (Ray o d) =
    -- Setting up the quadratic equation coefficients
    let a = d .*. d
        b = 2 .* oc .*. d
        c = oc .*. oc - (r * r)
        oc = o - sc

    -- Quadratic formula in action
        discr = b * b - (4 * a * c)
        sqroot = sqrt discr / (2 * a)
        bo2a  = negate b / (2 * a)
        solns = filter (> 0) [bo2a + sqroot, bo2a - sqroot]
        t = minimum solns
    in if discr <= 0 || null solns
          then Nothing
          else Just (t, ())

-- #419begin
--   #type=1
--   #src=https://en.wikipedia.org/wiki/Möller-Trumbore_intersection_algorithm
-- #419end
intersectWithRay (Triangle v1 v2 v3) (Ray o d) =
    let e1   = v2 - v3
        e2   = v3 - v1
        p    = d `cross` e2
        det  = e1 .*. p
        invd = 1 / det
        dt   = o - v1
        u    = (dt .*. p) * invd
        q    = dt `cross` e1
        v    = (d .*. q) * invd
        t    = (e2 .*. q) * invd
    in if isZero det || u < 0 || u > 1 || v < 0 || u + v > 1 || t <= 0
          then Nothing
          else Just (t, ())

-- Given a point on the Shape, compute the normal direction at that location
computeNormal :: Shape -> V3 -> V3

-- Planes have a uniform normal
computeNormal (Plane normal _) _ = normal

-- Just subtract the intersection point by the sphere center
computeNormal (Sphere c _) p = normalize (p - c)

-- Triangles also have a uniform normal.
-- The edges are v1 -> v2, v2 -> v3, v3 -> v1 (positive orientation)
-- so we use the cross product of the first two edges.
computeNormal (Triangle v1 v2 v3) _ = normalize $ (v2 - v1) `cross` (v3 - v2)
