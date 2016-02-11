module Ray.Geometry where

-- Convenient float operation
isZero :: Double -> Bool
isZero d = abs d <= 1e-10

-- A 3D vector containing double-precision floating-point coordinates
data V3 = V Double Double Double deriving (Eq, Ord, Show)

-- Multiplication by a scalar
(.*) :: Double -> V3 -> V3
k .* v = V k k k * v

-- Multiplication by a scalar
(*.) :: V3 -> Double -> V3
(*.) = flip (.*)

-- Dot product
(.*.) :: V3 -> V3 -> Double
l .*. r = case l * r of
            (V x y z) -> x + y + z

-- Cross product
cross :: V3 -> V3 -> V3
cross (V u1 u2 u3) (V v1 v2 v3) = V (u2*v3 - u3*v2) (u3*v1 - u1*v3) (u1*v2 - u2*v1)

-- Vector magnitude
magnitude :: V3 -> Double
magnitude v = sqrt (v .*. v)

-- Normalize vector to unit length
normalize :: V3 -> V3
normalize v = v *. (1 / magnitude v)

-- Normalize vector to specific length
scaledTo :: V3 -> Double -> V3
scaledTo v k = normalize v *. k


-- Nifty little trick for us to obtain arithmetic operators on 3D vectors
instance Num V3 where
  (V u v w) + (V x y z) = V (u+x) (v+y) (w+z)
  (V u v w) - (V x y z) = V (u-x) (v-y) (w-z)
  (V u v w) * (V x y z) = V (u*x) (v*y) (w*z)
  -- (V u v w) - (V x y z) = V (u-x) (v-y) (w-z)
  abs    (V x y z)      = V (abs x) (abs y) (abs z)
  signum (V x y z)      = V (signum x) (signum y) (signum z)
  fromInteger x         = V x' x' x'
    where x' = fromInteger x

-- Nifty little trick for us to obtain averaging 3D Vectors
instance Fractional V3 where
  (V u v w) / (V x y z) = V (u/x) (v/y) (w/z)
  recip (V x y z)       = V (recip x) (recip y) (recip z)
  fromRational x        = V x' x' x'
    where x' = fromRational x


-- A Ray consists of two vectors: a starting position and a direction
data Ray = Ray V3 V3 deriving (Eq, Ord, Show)

-- Smart constructor
ray :: V3 -> V3 -> Ray
ray pos dir = Ray pos (normalize dir)


data Shape = Plane V3 Double   -- A normal direction and a coefficient
           | Sphere V3 Double  -- A center point and a radius
           | Triangle V3 V3 V3 -- Three points in space (right-hand orientation)
           deriving (Eq, Ord, Show)

planeFromNormalAndPoint :: V3 -> V3 -> Shape
planeFromNormalAndPoint n p = Plane n' (negate $ n' .*. p)
  where n' = normalize n

-- A shape must be able to decide where it is intersected by a Ray
-- If successful, returns the distance along the ray where the intersection occurs
intersectWithRay :: Shape -> Ray -> Maybe Double

-- Algorithm ripped from Lecture-2
intersectWithRay (Plane n d) (Ray p v) =
    let vDotN = v .*. n
        t     = negate (p .*. n + d) / vDotN
    in if isZero vDotN || t <= 0
          then Nothing
          else Just t

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
          else Just t

-- Algorithm ripped from Wikipedia - Moller-Trumbore Intersection Algorithm
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
          else Just t


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
