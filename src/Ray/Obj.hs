{-# LANGUAGE OverloadedStrings #-}

module Ray.Obj
  (
    VertIndex
  , Face(..)
  , Bounds(..)
  , Octree(..)
  , Contains(..)
  , bContains
  , Mesh(..)
  , readInMesh
  ) where

import Ray.Geometry

import qualified Data.Attoparsec.Text as Parse
import qualified Data.Text.IO as Text
import Data.Text (Text)
import qualified Data.Vector as Vector
import Data.Vector (Vector, (!))

import Debug.Trace

-- An index into an array of vertices
type VertIndex = Int

-- A face is a triplet of vertex indices into an array of vertices
data Face = Face VertIndex VertIndex VertIndex deriving (Eq, Show)

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
                , z1     :: Octree a
                , z2     :: Octree a
                , z3     :: Octree a
                , z4     :: Octree a
                , z5     :: Octree a
                , z6     :: Octree a
                , z7     :: Octree a
                , z8     :: Octree a
                , locals :: [a]
                } deriving (Eq, Show)

-- A triangular mesh is an array of vertices
-- and a spatial data structure for storing faces.
data Mesh = Mesh (Vector V3) (Octree Face) deriving (Show)

-- Whether or not the vertex is contained within the bounding box
bContainsPoint :: Bounds -> V3 -> Bool
bContainsPoint (Bounds (V a b c) (V u v w)) (V x y z) =
    and [ a <= x
        , x <= u
        , b <= y
        , y <= v
        , c <= z
        , z <= w
        ]

-- Whether or not the triangle specified by the triplet
-- of vertices is fully contained within the bounds
bContains :: Bounds -> (V3,V3,V3) -> Contains
bContains bounds (v1,v2,v3) =
    let cs = map (bContainsPoint bounds) [v1, v2, v3]
        and' = and cs
        or'  = or cs
    in if and'
       then Contains
       else if or'
            then Partial
            else NotContains


-- Given a FilePath, open up that file, and try to produce a Mesh from it,
-- assuming it's a valid OBJ file
readInMesh :: FilePath -> IO (Either String Mesh)
readInMesh fp = do fileContents <- Text.readFile fp
                   let parser = parseMesh
                   return $ Parse.parseOnly parser fileContents

-- A parser to pull out a Mesh value
parseMesh :: Parse.Parser Mesh
parseMesh = do vertices <- parseVertices
               faces    <- parseFaces
               return $ Mesh vertices $ buildOctree vertices faces

-- A parser to pull out an array of vertices
parseVertices :: Parse.Parser (Vector.Vector V3)
parseVertices = Vector.fromList . map toV3 <$> parseManyKeyLines "v" Parse.double
  where toV3 (a, b, c) = V a b c

-- A parser to pull out a list of faces
parseFaces :: Parse.Parser [Face]
parseFaces = map toFace <$> parseManyKeyLines "f" Parse.decimal
  where toFace (a, b, c) = Face (a-1) (b-1) (c-1) -- fix to use zero-based indexing

-- Helper function that takes a string "something"
-- and a parser p designed for a line like:
--     "something p p p\n"
-- and pulls out all the lines with each of the three p values.
--
-- For example, for parseVertices, we have string "v" and floating values
-- parseManyKeyLines "v" Parse.double takes:
--   """v 1.0 2.0 3.0\n
--      v 4.0 5.0 6.0\n"""
-- and returns [(1.0, 2.0, 3.0), (4.0, 5.0, 6.0)]
parseManyKeyLines :: Text -> Parse.Parser a -> Parse.Parser [(a, a, a)]
parseManyKeyLines key p = Parse.many1 lineParse
  where lineParse = do Parse.skipSpace
                       _ <- Parse.string key
                       Parse.skipSpace
                       a <- p
                       Parse.skipSpace
                       b <- p
                       Parse.skipSpace
                       c <- p
                       return (a, b, c)


-- O(V + FlogF) Octree construction algorithm
-- where V is the #vertices and F is the #faces
buildOctree :: Vector V3 -> [Face] -> Octree Face
buildOctree verts faces = buildOctree' verts faces (computeBounds verts)

-- Reads through all vertices and finds an axis-aligned
-- bounding box that contains them all
computeBounds :: Vector V3 -> Bounds
computeBounds verts =
    let len         = Vector.length verts
        origin      = V 0 0 0
        emptyBounds = Bounds origin origin

        go i bounds| i < len   =
            let Bounds (V a b c) (V u v w) = bounds
                V x y z                    = verts ! i
                a' = min a x
                b' = min b y
                c' = min c z
                u' = max u x
                v' = max v y
                w' = max w z
            in go (i+1) $ Bounds (V a' b' c') (V u' v' w')
        go _ bounds| otherwise = bounds
    in go 0 emptyBounds

type Octuple a   = (a, a, a, a, a, a, a, a)

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


-- Given the vertices, faces, and global bounds for the octree
-- compute the octree
buildOctree' :: Vector V3 -> [Face] -> Bounds -> Octree Face
buildOctree' _     []    bounds = Nil bounds
buildOctree' verts faces bounds =
    let updateState :: Face -> OctreeBuild Face -> OctreeBuild Face
        updateState f@(Face i1 i2 i3) ob' =
            let -- Compute the actual vertices from the vertex indices
                tri = (verts ! i1, verts ! i2, verts ! i3)

                check acc ob =
                    -- If we have not yet found an octant for this face:
                    case dirtyBit ob of
                      Contains    -> ob -- already contained in another octant
                      Partial     -> ob -- partially contained by another octant
                      -- Alright, go for it. Test for containment
                      NotContains -> case bContains (snd $ acc ob) tri of
                                       NotContains -> ob -- this octant doesn't contain it
                                       Contains    -> conc acc ob{dirtyBit = Contains}
                                       Partial     -> ob{dirtyBit = Partial} -- partial contain

                -- Using the accessor/setter acc, add Face f
                -- to the octant associated with acc.
                conc acc ob = fst $ acc ob

                -- Check the dirty bit
                --   NotContains/Partial imply we haven't added Face f to any octant,
                --     so add it to the local octant
                --   Contains implies we put it in an octant already
                flush ob = case dirtyBit ob of
                             NotContains -> ob{le = f : le ob}
                             Partial     -> ob{le = f : le ob, dirtyBit = NotContains}
                             _           -> ob{dirtyBit = NotContains}

                -- A bunch of accessor/setter functions
                --  The first element concatenates the Face f
                --     to the associated list
                --  The second element retrieves the Bounds
                --     for the associated list
                _1 ob = (ob{l1 = f : l1 ob}, bs1 ob)
                _2 ob = (ob{l2 = f : l2 ob}, bs2 ob)
                _3 ob = (ob{l3 = f : l3 ob}, bs3 ob)
                _4 ob = (ob{l4 = f : l4 ob}, bs4 ob)
                _5 ob = (ob{l5 = f : l5 ob}, bs5 ob)
                _6 ob = (ob{l6 = f : l6 ob}, bs6 ob)
                _7 ob = (ob{l7 = f : l7 ob}, bs7 ob)
                _8 ob = (ob{l8 = f : l8 ob}, bs8 ob)

            in foldr ($) ob' [
                 flush        -- or will it go with this local node? (applied last)
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
        bld = buildOctree' verts

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
        go (f':fs') ob = go fs' (updateState f' ob)

    in go faces octreeBuild


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

makeOctreeBuild :: Bounds -> OctreeBuild Face
makeOctreeBuild bounds =
    let (b1, b2, b3, b4, b5, b6, b7, b8) = splitBounds bounds
    in Ob [] [] [] [] [] [] [] [] [] b1 b2 b3 b4 b5 b6 b7 b8 NotContains
