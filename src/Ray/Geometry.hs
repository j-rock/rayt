module Ray.Geometry where

-- Convenient float operation
isZero :: Double -> Bool
isZero d = abs d <= 1e-10

-- A 3D vector containing double-precision floating-point coordinates
data V3 = V {-# UNPACK #-} !Double
            {-# UNPACK #-} !Double
            {-# UNPACK #-} !Double
          deriving (Eq, Ord, Show)

-- Multiplication by a scalar
(.*) :: Double -> V3 -> V3
k .* v = V k k k * v
{-# INLINE (.*) #-}

-- Multiplication by a scalar
(*.) :: V3 -> Double -> V3
(*.) = flip (.*)
{-# INLINE (*.) #-}

-- Dot product
(.*.) :: V3 -> V3 -> Double
(V xl yl zl) .*. (V xr yr zr) = xl*xr + yl*yr + zl*zr
{-# INLINE (.*.) #-}

-- Cross product
cross :: V3 -> V3 -> V3
cross (V u1 u2 u3) (V v1 v2 v3) = V (u2*v3 - u3*v2) (u3*v1 - u1*v3) (u1*v2 - u2*v1)
{-# INLINE cross #-}

-- Vector magnitude
magnitude :: V3 -> Double
magnitude (V x y z) = sqrt (x*x + y*y + z*z)
{-# INLINE magnitude #-}

-- Vector magnitude squared
sqrMagnitude :: V3 -> Double
sqrMagnitude (V x y z) = x*x + y*y + z*z
{-# INLINE sqrMagnitude #-}

-- Normalize vector to unit length
normalize :: V3 -> V3
normalize v = (1 / magnitude v) .* v
{-# INLINE normalize #-}

-- Normalize vector to specific length
scaledTo :: V3 -> Double -> V3
scaledTo v k = normalize v *. k
{-# INLINE scaledTo #-}

-- #419begin
--   #type=1
--   #src=https://en.wikipedia.org/wiki/Phong_reflection_model
-- #419end
-- Reflect the first vector over the second one. Assumes unit length vectors.
-- Excuse me for forgetting the most basic of linear algebraic operations.
reflectedOver :: V3 -> V3 -> V3
reflectedOver l n = 2 * (l .*. n) .* n - l

-- Clamps vectors component-wise.
-- Pass in min, max, and the argument vector.
clampV :: V3 -> V3 -> V3 -> V3
clampV (V mnx mny mnz) (V mxx mxy mxz) (V x y z) = V x' y' z'
  where x' = min mxx (max mnx x)
        y' = min mxy (max mny y)
        z' = min mxz (max mnz z)

-- Nifty little trick for us to obtain arithmetic operators on 3D vectors
instance Num V3 where
  (V u v w) + (V x y z) = V (u+x) (v+y) (w+z)
  {-# INLINE (+) #-}
  (V u v w) - (V x y z) = V (u-x) (v-y) (w-z)
  {-# INLINE (-) #-}
  (V u v w) * (V x y z) = V (u*x) (v*y) (w*z)
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

getRayDir :: Ray -> V3
getRayDir (Ray _ dir) = dir
