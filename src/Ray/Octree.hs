{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ray.Octree where

import Ray.Geometry
import Ray.Intersection
import qualified Data.Maybe as Maybe

-- Bottom-left-back and Top-right-front corners of a cube
-- An axis-aligned bounding box
data Bounds = Bounds V3 V3 deriving (Eq, Show)

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
aabbContains :: Bounds -> Bounds -> Bool
aabbContains (Bounds (V bx by bz) (V bu bv bw)) bounds =
    let Bounds (V x y z) (V u v w) = bounds
        preds = [ bx <= x
                , by <= y
                , bz <= z
                , bu >= u
                , bv >= v
                , bw >= w
                ]
    in and preds

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

-- Creates an AABB that can fit both argument AABBs
aabbExpand :: Bounds -> Bounds -> Bounds
aabbExpand (Bounds (V a b c) (V d e f)) (Bounds (V g h i) (V j k l)) =
    let minV = V (min a g) (min b h) (min c i)
        maxV = V (max d j) (max e k) (max f l)
    in Bounds minV maxV


type Octuple a   = (a, a, a, a, a, a, a, a)

octMap :: (a -> b) -> Octuple a -> Octuple b
octMap f (a,b,c,d,e,f',g,h) = (f a, f b, f c, f d, f e, f f', f g, f h)

octupleToList :: Octuple a -> [a]
octupleToList (a,b,c,d,e,f,g,h) = [a,b,c,d,e,f,g,h]

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

octreeFromList :: AABB a => Dict a -> [a] -> Octree a
octreeFromList d objs = buildOctree d objs bounds
  where bounds = let initialBounds = Bounds (V 0 0 0) (V 0 0 0)
                     update bnds obj = aabbExpand bnds (getBounds d obj)
                 in foldl update initialBounds objs

buildOctree :: AABB a => Dict a -> [a] -> Bounds -> Octree a
buildOctree _ []   bounds = Nil bounds
buildOctree _ objs bounds|lenLeq 5 objs =
    let (z1, z2, z3, z4, z5, z6, z7, z8) = octMap Nil $ splitBounds bounds
    in Node bounds z1 z2 z3 z4 z5 z6 z7 z8 objs
buildOctree d objs bounds =
    let splits = splitBounds bounds

        contains aBounds = octupleToList $ octMap (`aabbContains` aBounds) splits

        updateState a ob = let aBounds = getBounds d a
                               allContains = contains aBounds
                               updates = [ ob{l1 = a : l1 ob}
                                         , ob{l2 = a : l2 ob}
                                         , ob{l3 = a : l3 ob}
                                         , ob{l4 = a : l4 ob}
                                         , ob{l5 = a : l5 ob}
                                         , ob{l6 = a : l6 ob}
                                         , ob{l7 = a : l7 ob}
                                         , ob{l8 = a : l8 ob}
                                         ]
                               withUpd = zip allContains updates
                               containers = filter fst withUpd
                           in if length containers == 1
                              then snd . head $ containers
                              else ob{le = a : le ob}

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
                     }

makeOctreeBuild :: Bounds -> OctreeBuild a
makeOctreeBuild bounds =
    let (b1, b2, b3, b4, b5, b6, b7, b8) = splitBounds bounds
    in Ob [] [] [] [] [] [] [] [] [] b1 b2 b3 b4 b5 b6 b7 b8


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

getOctreeBounds :: Octree a -> Bounds
getOctreeBounds (Nil b)    = b
getOctreeBounds Node{aabb} = aabb
