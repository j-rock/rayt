module Ray.Affine
    ( AffineTransformation
    , Axis(..)
    , translateA
    , scaleA
    , rotateA
    , reflectA
    , AffineBijection
    , projectPoint
    , invertRay
    , invertNormal
    , mkAffine
    , noAffineTrans
    ) where

import Ray.Geometry

-- Used for rotations
data Axis = X | Y | Z deriving (Eq, Show)

data AffineTransformation = Translate V3 -- translate by a vector
                          | Scale V3 -- scale each dimension by its associated component
                          | Rotation Axis Double -- Rotate over X, Y, Z by degrees
                          | Reflect Axis -- Reflect over an axis
                          | TranslateTranspose V3 -- exists to make life easier
                                                  -- but not exposed to user
-- Functions hide implementation
translateA :: V3 -> AffineTransformation
translateA = Translate

scaleA :: V3 -> AffineTransformation
scaleA = Scale

rotateA :: Axis -> Double -> AffineTransformation
rotateA = Rotation

reflectA :: Axis -> AffineTransformation
reflectA = Reflect

invertTrans :: AffineTransformation -> AffineTransformation
invertTrans (Translate v)     = Translate $ negate v
invertTrans (Scale v)         = Scale $ 1 / v
invertTrans (Rotation ax ang) = Rotation ax $ negate ang
invertTrans (Reflect ax)      = Reflect ax
invertTrans (TranslateTranspose v) = TranslateTranspose $ negate v

transposeTrans :: AffineTransformation -> AffineTransformation
transposeTrans (Translate v)          = TranslateTranspose v
transposeTrans (TranslateTranspose v) = Translate v
transposeTrans (Scale v)       = Scale v
transposeTrans (Reflect ax)    = Reflect ax
transposeTrans x               = invertTrans x


applyTrans :: AffineTransformation -> V4 -> V4
-- Using w = 0,1 assumption
applyTrans (Translate _           ) v@(V4 _ _ _ 0) = v
applyTrans (Translate (V tx ty tz))   (V4 x y z w) = V4 (tx+x) (ty+y) (tz+z) w

applyTrans (TranslateTranspose (V tx ty tz)) (V4 x y z w)  = V4 x y z (tx*x + ty*y + tz*z + w)

applyTrans (Scale (V sx sy sz)) (V4 x y z w) = V4 (sx*x) (sy*y) (sz*z) w

applyTrans (Reflect ax) (V4 x y z w)|ax == X   = V4 (-x)  y   z w
                                    |ax == Y   = V4   x (-y)  z w
                                    |otherwise = V4   x   y (-z) w

applyTrans (Rotation ax ang) (V4 x y z w)|ax == X   = V4 x (c*y - s*z) (s*y + c*z) w
                                         |ax == Y   = V4 (c*x + s*z) y (c*z - s*x) w
                                         |otherwise = V4 (c*x - s*y) (s*x + c*y) z w
  where (c, s) = let t = ang * pi / 180
                 in (cos t, sin t)


buildFunc :: [AffineTransformation] -> V4 -> V3
buildFunc ats v = let (V4 x y z _) = foldr applyTrans v ats
                  in V x y z -- technically, one should divide by w
                             -- but w = 0 or 1

data AffineBijection = AffineBijection {
                         project :: V4 -> V3 -- T(v)
                       , invert  :: V4 -> V3 -- T^(-1) (v)
                       , invert' :: V4 -> V3 -- T'^(-1) (v)
                       }

instance Show AffineBijection where
  show = const "AffineBijection"

mkAffine :: [AffineTransformation] -> AffineBijection
mkAffine ts =
    let mats  = ts
        imats = reverse $ map invertTrans ts
        tmats = map (invertTrans . transposeTrans) ts

    in AffineBijection {
         project = buildFunc mats
       , invert  = buildFunc imats
       , invert' = buildFunc tmats
       }


projectPoint :: AffineBijection -> V3 -> V3
projectPoint ab (V x y z) = project ab $ V4 x y z 1
{-# INLINE projectPoint #-}

invertRay :: AffineBijection -> Ray -> Ray
invertRay ab (Ray (V ox oy oz) (V dx dy dz)) =
    let o' = invert ab (V4 ox oy oz 1)
        d' = invert ab (V4 dx dy dz 0)
    in Ray o' d'
{-# INLINE invertRay #-}

invertNormal :: AffineBijection -> V3 -> V3
invertNormal ab (V x y z) = invert' ab $ V4 x y z 0
{-# INLINE invertNormal #-}

noAffineTrans :: AffineBijection
noAffineTrans = mkAffine []
