{-# LANGUAGE TypeFamilies #-}

module Ray.Shape where

import           Ray.Geometry
import           Ray.Octree
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
--   #src=http://www.gamedev.net/topic/481835-3d-point-in-triangle-algorithm/
-- #419end
intersectWithRay tri@(Triangle v1 v2 v3) r@(Ray o d) =
    let triPlane = planeFromNormalAndPoint (computeNormal tri v1) v1

        isPointInTri p =
            let area p1 p2 p3 = magnitude $ (p2-p1) `cross` (p3 - p1)
                aTri = area v1 v2 v3
                a1   = area v1 v2 p / aTri
                a2   = area v2 v3 p / aTri
                a3   = area v3 v1 p / aTri
            in and [ all (> 0) [a1,a2,a3]
                   , all (< 1) [a1,a2,a3]
                   , abs (1 - (a1 + a2 + a3)) < 1e-10
                   ]

    in  case intersectWithRay triPlane r of
            Nothing     -> Nothing
            Just (t, _) -> if isPointInTri (o + t .* d)
                           then Just (t, ())
                           else Nothing

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



-- Returns an AABB for the given Shape
getShapeBounds :: Shape -> Bounds

-- Planes are infinite, man
getShapeBounds (Plane _ _) = Bounds (negate huge) huge
  where huge   = V bigNum bigNum bigNum
        bigNum = 1e15

-- Move out one radius
getShapeBounds (Sphere c r) = Bounds (c-rV) (c+rV)
  where rV = V r r r

getShapeBounds (Triangle v1 v2 v3) =
    let V x1 y1 z1 = v1
        V x2 y2 z2 = v2
        V x3 y3 z3 = v3

        minV = V (minimum [x1,x2,x3]) (minimum [y1,y2,y3]) (minimum [z1,z2,z3])
        maxV = V (maximum [x1,x2,x3]) (maximum [y1,y2,y3]) (maximum [z1,z2,z3])

    in Bounds minV maxV
