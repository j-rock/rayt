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


type Sixteentuple a = (a,a,a,a,  a,a,a,a,  a,a,a,a,  a,a,a,a)
type AffMat = Sixteentuple Double

asMatrix :: AffineTransformation -> AffMat
asMatrix (Translate (V x y z))  = (1,0,0,x,  0,1,0,y,  0,0,1,z,  0,0,0,1)
asMatrix (TranslateTranspose v) = matTranspose $ asMatrix $ Translate v
asMatrix (Scale (V x y z))      = (x,0,0,0,  0,y,0,0,  0,0,z,0,  0,0,0,1)

asMatrix (Rotation X ang) = (1,0,0,0,  0,c,n,0,  0,s,c,0,  0,0,0,1)
  where (c, s, n) = computeDegree ang
asMatrix (Rotation Y ang) = (c,0,s,0,  0,1,0,0,  n,0,c,0,  0,0,0,1)
  where (c, s, n) = computeDegree ang
asMatrix (Rotation Z ang) = (c,n,0,0,  s,c,0,0,  0,0,1,0,  0,0,0,1)
  where (c, s, n) = computeDegree ang

asMatrix (Reflect X) = (-1,0,0,0,  0, 1,0,0,  0,0, 1,0,  0,0,0,1)
asMatrix (Reflect Y) = ( 1,0,0,0,  0,-1,0,0,  0,0, 1,0,  0,0,0,1)
asMatrix (Reflect Z) = ( 1,0,0,0,  0, 1,0,0,  0,0,-1,0,  0,0,0,1)

computeDegree :: Double -> (Double, Double, Double)
computeDegree deg = let t = deg * pi / 180
                        s = sin t
                    in (cos t, s, negate s)

matTranspose :: AffMat -> AffMat
matTranspose (a1,a2,a3,a4, b1,b2,b3,b4, c1,c2,c3,c4, d1,d2,d3,d4) =
    ( a1,b1,c1,d1
    , a2,b2,c2,d2
    , a3,b3,c3,d3
    , a4,b4,c4,d4
    )

matMult :: AffMat -> AffMat -> AffMat
matMult (a1,a2,a3,a4, b1,b2,b3,b4, c1,c2,c3,c4, d1,d2,d3,d4)
        (e1,e2,e3,e4, f1,f2,f3,f4, g1,g2,g3,g4, h1,h2,h3,h4) =
    let a1' = quadDot a1 a2 a3 a4 e1 f1 g1 h1
        a2' = quadDot a1 a2 a3 a4 e2 f2 g2 h2
        a3' = quadDot a1 a2 a3 a4 e3 f3 g3 h3
        a4' = quadDot a1 a2 a3 a4 e4 f4 g4 h4

        b1' = quadDot b1 b2 b3 b4 e1 f1 g1 h1
        b2' = quadDot b1 b2 b3 b4 e2 f2 g2 h2
        b3' = quadDot b1 b2 b3 b4 e3 f3 g3 h3
        b4' = quadDot b1 b2 b3 b4 e4 f4 g4 h4

        c1' = quadDot c1 c2 c3 c4 e1 f1 g1 h1
        c2' = quadDot c1 c2 c3 c4 e2 f2 g2 h2
        c3' = quadDot c1 c2 c3 c4 e3 f3 g3 h3
        c4' = quadDot c1 c2 c3 c4 e4 f4 g4 h4

        d1' = quadDot d1 d2 d3 d4 e1 f1 g1 h1
        d2' = quadDot d1 d2 d3 d4 e2 f2 g2 h2
        d3' = quadDot d1 d2 d3 d4 e3 f3 g3 h3
        d4' = quadDot d1 d2 d3 d4 e4 f4 g4 h4

    in (a1',a2',a3',a4', b1',b2',b3',b4', c1',c2',c3',c4', d1',d2',d3',d4')


quadDot :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
quadDot a1 a2 a3 a4 b1 b2 b3 b4 = a1*b1 + a2*b2 + a3*b3 + a4*b4
{-# INLINE quadDot #-}

applyMat :: AffMat -> V4 -> V3       -- Last row collapsed
applyMat (a1,a2,a3,a4, b1,b2,b3,b4, c1,c2,c3,c4, _,_,_,_) (V4 x y z w) =
    let x' = a1*x + a2*y + a3*z + a4*w
        y' = b1*x + b2*y + b3*z + b4*w
        z' = c1*x + c2*y + c3*z + c4*w
    in V x' y' z'

buildFunc :: [AffMat] -> V4 -> V3
-- Technically, one should divide by w, but w always = 0 or 1
buildFunc []       (V4 x y z _) = V x y z
buildFunc [m]       v           = applyMat m v
buildFunc (m:m':ms) v           = buildFunc (matMult m m' : ms) v


data AffineBijection = AffineBijection {
                         project :: V4 -> V3 -- T(v)
                       , invert  :: V4 -> V3 -- T^(-1) (v)
                       , invert' :: V4 -> V3 -- T'^(-1) (v)
                       }

instance Show AffineBijection where
  show = const "AffineBijection"

mkAffine :: [AffineTransformation] -> AffineBijection
mkAffine ts =
    let mats  = map asMatrix ts
        imats = reverse $ map (asMatrix . invertTrans) ts
        tmats = map (asMatrix . invertTrans . transposeTrans) ts

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
