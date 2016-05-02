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

planeFromUnitNormalAndPoint :: V3 -> V3 -> Shape
planeFromUnitNormalAndPoint n p = Plane n (negate $ n .*. p)
{-# INLINE planeFromUnitNormalAndPoint #-}

planeFromNormalAndPoint :: V3 -> V3 -> Shape
planeFromNormalAndPoint n = planeFromUnitNormalAndPoint $ normalize n
{-# INLINE planeFromNormalAndPoint #-}

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
        b = 2 * (oc .*. d)
        c = oc .*. oc - (r * r)
        oc = o - sc

    -- Quadratic formula in action
        discr = b * b - (4 * a * c)
        twoA = 2 * a
        sqroot = sqrt discr / twoA
        bo2a  = negate b / twoA
        solns = takeWhile (>= 0) [bo2a + sqroot, bo2a - sqroot]
        t = minimum solns

    in if discr <= 0 || null solns
       then Nothing
       else Just (t, ())

-- #419begin
--   #type=1
--   #src=http://www.gamedev.net/topic/481835-3d-point-in-triangle-algorithm/
-- #419end
intersectWithRay (Triangle v1 v2 v3) r@(Ray o d) =
    let v13 = v1 - v3
        v21 = v2 - v1
        v32 = v3 - v2

        c2132 = v21 `cross` v32

        -- c2132 might not actually be unit, but we only
        -- construct the plane for a quick intersection.
        -- Cheaper to not have to normalize
        triPlane = planeFromUnitNormalAndPoint c2132 v1

        isPointInTri p =
            let sarea v ps pt = sqrMagnitude $ v `cross` (pt - ps)
                saTri = sqrMagnitude c2132
                sa1   = sarea v21 v1 p / saTri
                sa2   = sarea v32 v2 p / saTri
                sa3   = sarea v13 v3 p / saTri

                a1 = sqrt sa1
                a2 = sqrt sa2
                a3 = sqrt sa3

            in and [ a1 > 0, a2 > 0, a3 > 0
                   , a1 < 1, a2 < 1, a3 < 1
                   , abs (1 - (a1 + a2 + a3)) < 1e-10
                   ]

    in  case intersectWithRay triPlane r of
            Nothing     -> Nothing
            Just (t, _) -> if isPointInTri (o + t .* d)
                           then Just (t, ())
                           else Nothing

-- Given a point on the Shape, compute the normal direction at that location.
-- It does not need to be normalized.
computeNormal :: Shape -> V3 -> V3

-- Planes have a uniform normal
computeNormal (Plane normal _) _ = normal

-- Just subtract the intersection point by the sphere center
computeNormal (Sphere c _) p = p - c

-- Triangles also have a uniform normal.
-- The edges are v1 -> v2, v2 -> v3, v3 -> v1 (positive orientation)
-- so we use the cross product of the first two edges.
computeNormal (Triangle v1 v2 v3) _ = (v2 - v1) `cross` (v3 - v2)



-- Returns an AABB for the given Shape
getShapeBounds :: Shape -> Bounds

-- Planes are infinite, man
getShapeBounds (Plane _ _) = Bounds (negate huge) huge
  where huge   = V bigNum bigNum bigNum
        bigNum = 1e15

-- Move out one radius
getShapeBounds (Sphere c r) = Bounds (c-rV) (c+rV)
  where rV = V r r r

getShapeBounds (Triangle (V x1 y1 z1) (V x2 y2 z2) (V x3 y3 z3)) =
    let minV = V (minimum [x1,x2,x3]) (minimum [y1,y2,y3]) (minimum [z1,z2,z3])
        maxV = V (maximum [x1,x2,x3]) (maximum [y1,y2,y3]) (maximum [z1,z2,z3])

    in Bounds minV maxV

-- U,V coordinates for a given position on a shape
-- #419begin
--   #type=1
--   #src=http://stackoverflow.com/questions/18663755/how-to-convert-a-3d-point-on-a-plane-to-uv-coordinates
-- #419end
getShapeUV :: Shape -> V3 -> (Double, Double)
getShapeUV (Plane n _) p =
    let (u, v) = getUVBasisForNormal n
    in (u .*. p, v .*. p)



-- #419begin
--   #type=3
--   #src=https://en.wikipedia.org/wiki/UV_mapping
-- #419end
getShapeUV (Sphere c _) p =
    let V x y z = normalize $ (p - c)
        u = 0.5 + atan2 z x
        v = 0.5 - asin y
    in (u, v)

getShapeUV (Triangle v1 v2 v3) p =
    let triPlane = planeFromNormalAndPoint (v2 - v1 `cross` v3 - v2) v1
    in getShapeUV triPlane p
