{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Ray.Octree where

import Ray.Geometry
import Ray.Intersection
import qualified Data.Maybe as Maybe

-- Bottom-left-back and Top-right-front corners of a cube
-- An axis-aligned bounding box
data Bounds = Bounds V3 V3 deriving (Eq, Show)

-- Simple sum type for bounding box containment
data Contains = NotContains
              | Contains
              | Partial
              deriving (Eq, Show)

-- Octree for fast ray-triangle intersection
data Octree a = Nil Bounds
              | Node {
                  aabb   :: Bounds
                , zone1     :: Octree a
                , zone2     :: Octree a
                , zone3     :: Octree a
                , zone4     :: Octree a
                , zone5     :: Octree a
                , zone6     :: Octree a
                , zone7     :: Octree a
                , zone8     :: Octree a
                , locals :: [a]
                } deriving (Eq, Show)

class AABB a where
  type Dict a :: *
  getBounds :: Dict a -> a -> Bounds

-- Whether or not the first Bounds passed in contain the second Bounds
aabbContains :: Bounds -> Bounds -> Contains
aabbContains (Bounds (V bx by bz) (V bu bv bw)) bounds =
    let Bounds (V x y z) (V u v w) = bounds
        preds = [ bx <= x
                , by <= y
                , bz <= z
                , bu >= u
                , bv >= v
                , bw >= w
                ]
    in if and preds
       then Contains
       else if or preds
            then Partial
            else NotContains

-- #419begin
--   #type=1
--   #src=http://gamedev.stackexchange.com/questions/18436/most-efficient-aabb-vs-ray-collision-algorithms
-- #419end
aabbContainsRay :: Bounds -> Ray -> Bool
aabbContainsRay (Bounds (V bx by bz) (V tx ty tz)) (Ray (V ox oy oz) rayDir) =
    let V dfx dfy dfz = V 1.0 1.0 1.0 / rayDir
        f a b c = (a - b) * c
        t1      = f bx ox dfx
        t2      = f tx ox dfx
        t3      = f by oy dfy
        t4      = f ty oy dfy
        t5      = f bz oz dfz
        t6      = f tz oz dfz
        tMin = max (max (min t1 t2) (min t3 t4)) (min t5 t6)
        tMax = min (min (max t1 t2) (max t3 t4)) (max t5 t6)
    in tMax >= 0 && tMin < tMax


type Octuple a   = (a, a, a, a, a, a, a, a)

octMap :: (a -> b) -> Octuple a -> Octuple b
octMap f (a,b,c,d,e,f',g,h) = (f a, f b, f c, f d, f e, f f', f g, f h)

-- Divides an AABB into 8 uniform AABB's.
splitBounds :: Bounds -> Octuple Bounds
splitBounds (Bounds bl@(V a b c) tr@(V u v w)) =
    let x = (u - a) / 2
        y = (v - b) / 2
        z = (w - c) / 2
        right = V x 0 0
        up    = V 0 y 0
        front = V 0 0 z

        mkSection pt = Bounds pt (pt + V x y z)

        blr  = bl + right
        blf  = bl + front
        blrf = blr + front

        blu    = bl + up
        blru   = blu + right
        blfu   = blu + front
        center = blru + front

        z1' = Bounds bl center
        z2' = mkSection blr
        z3' = mkSection blf
        z4' = mkSection blrf

        z5' = mkSection blu
        z6' = mkSection blru
        z7' = mkSection blfu
        z8' = Bounds center tr

    in (z1', z2', z3', z4', z5', z6', z7', z8')

lenLeq :: Int -> [a] -> Bool
lenLeq k [] = k >= 0
lenLeq k (_:fs) = k >= 0 && lenLeq (k-1) fs


buildOctree :: AABB a => Dict a -> [a] -> Bounds -> Octree a
buildOctree _ []   bounds = Nil bounds
buildOctree _ objs bounds|lenLeq 5 objs =
    let (z1, z2, z3, z4, z5, z6, z7, z8) = octMap Nil $ splitBounds bounds
    in Node bounds z1 z2 z3 z4 z5 z6 z7 z8 objs
buildOctree d objs bounds =
    let updateState a ob' =
            let aBounds = getBounds d a
                check acc ob =
                    -- If we have not yet found an octant for this face:
                    case dirtyBit ob of
                      Contains    -> ob -- already contained in another octant
                      Partial     -> ob -- partially contained by another octant
                      -- Alright, go for it. Test for containment
                      NotContains -> case aabbContains (snd $ acc ob) aBounds of
                                       NotContains -> ob -- this octant doesn't contain it
                                       Contains    -> conc acc ob{dirtyBit = Contains}
                                       Partial     -> ob{dirtyBit = Partial} -- partial contain

                -- Using the accessor/setter acc, add value a
                -- to the octant associated with acc.
                conc acc ob = fst $ acc ob

                -- Check the dirty bit
                --   NotContains/Partial imply we haven't added value a to any octant,
                --     so add it to the local octant
                --   Contains implies we put it in an octant already
                flush ob = case dirtyBit ob of
                             NotContains -> ob{le = a : le ob}
                             Partial     -> ob{le = a : le ob, dirtyBit = NotContains}
                             _           -> ob{dirtyBit = NotContains}

                -- A bunch of accessor/setter functions
                --  The first element concatenates the value a
                --     to the associated list
                --  The second element retrieves the Bounds
                --     for the associated list
                _1 ob = (ob{l1 = a : l1 ob}, bs1 ob)
                _2 ob = (ob{l2 = a : l2 ob}, bs2 ob)
                _3 ob = (ob{l3 = a : l3 ob}, bs3 ob)
                _4 ob = (ob{l4 = a : l4 ob}, bs4 ob)
                _5 ob = (ob{l5 = a : l5 ob}, bs5 ob)
                _6 ob = (ob{l6 = a : l6 ob}, bs6 ob)
                _7 ob = (ob{l7 = a : l7 ob}, bs7 ob)
                _8 ob = (ob{l8 = a : l8 ob}, bs8 ob)

            in foldr ($) ob' [
                 flush        -- Will it go with this local node? (applied last)
               , check _1     -- does it go in the 1st octant?
               , check _2     -- second octant?
               , check _3     -- etc.
               , check _4
               , check _5
               , check _6
               , check _7
               , check _8
             ]

        octreeBuild = makeOctreeBuild bounds
        bld = buildOctree d

        go []     ob = Node
                         bounds
                         (bld (l1 ob) (bs1 ob))
                         (bld (l2 ob) (bs2 ob))
                         (bld (l3 ob) (bs3 ob))
                         (bld (l4 ob) (bs4 ob))
                         (bld (l5 ob) (bs5 ob))
                         (bld (l6 ob) (bs6 ob))
                         (bld (l7 ob) (bs7 ob))
                         (bld (l8 ob) (bs8 ob))
                         (le ob)
        go (f:fs) ob = go fs (updateState f ob)

    in go objs octreeBuild


-- Temporary data structure for building Octrees
data OctreeBuild a = Ob {
                       l1 :: [a]
                     , l2 :: [a]
                     , l3 :: [a]
                     , l4 :: [a]
                     , l5 :: [a]
                     , l6 :: [a]
                     , l7 :: [a]
                     , l8 :: [a]
                     , le :: [a]
                     , bs1 :: Bounds
                     , bs2 :: Bounds
                     , bs3 :: Bounds
                     , bs4 :: Bounds
                     , bs5 :: Bounds
                     , bs6 :: Bounds
                     , bs7 :: Bounds
                     , bs8 :: Bounds
                     , dirtyBit :: Contains
                     }

makeOctreeBuild :: Bounds -> OctreeBuild a
makeOctreeBuild bounds =
    let (b1, b2, b3, b4, b5, b6, b7, b8) = splitBounds bounds
    in Ob [] [] [] [] [] [] [] [] [] b1 b2 b3 b4 b5 b6 b7 b8 NotContains


instance Intersector Octree where
  getIntersects = getOctreeIntersects

getOctreeCoarseIntersects :: Octree a -> Ray -> [a]
getOctreeCoarseIntersects octree r =
    let -- Going to go through entire tree and thread list of coarse matches
        go as (Nil _) = as
        go as Node{..} =
            let as1 = locals ++ as
                as2 = go as1 zone1
                as3 = go as2 zone2
                as4 = go as3 zone3
                as5 = go as4 zone4
                as6 = go as5 zone5
                as7 = go as6 zone6
                as8 = go as7 zone7
                fin = go as8 zone8
            in if aabbContainsRay aabb r
               then fin
               else as

    in go [] octree

getOctreeIntersects :: IntersectObj a =>
                       Octree a -> Ray-> [(Double, a, IntersectionMetaData a)]
getOctreeIntersects octree r =
    let coarseMatches = getOctreeCoarseIntersects octree r
        -- Now filter the coarse matches for actual hits.
        intersects = Maybe.mapMaybe (intersectObjWithRay r) coarseMatches
    in intersects
