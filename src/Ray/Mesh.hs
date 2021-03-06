{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Ray.Mesh where

import           Ray.Geometry
import Ray.Intersection
import           Ray.Octree
import Ray.Shape

import qualified Data.Attoparsec.Text as Parse
import qualified Data.Either          as Either
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Text.IO         as TextIO
import           Data.Vector          (Vector)
import qualified Data.Vector          as Vector

import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Data.Ord (comparing)

-- An index into an array of vertices
type VertIndex = Int

-- A face is a triplet of vertex indices into an array of vertices
data Face = Face VertIndex VertIndex VertIndex deriving (Eq, Show)

instance AABB Face where
  type Dict Face = Vector V3
  getBounds verts (Face i1 i2 i3) =
      let V x1 y1 z1 = Vector.unsafeIndex verts i1
          V x2 y2 z2 = Vector.unsafeIndex verts i2
          V x3 y3 z3 = Vector.unsafeIndex verts i3
          minX = minimum [x1, x2, x3]
          minY = minimum [y1, y2, y3]
          minZ = minimum [z1, z2, z3]
          maxX = maximum [x1, x2, x3]
          maxY = maximum [y1, y2, y3]
          maxZ = maximum [z1, z2, z3]
      in Bounds (V minX minY minZ) (V maxX maxY maxZ)

-- A triangular mesh is an array of vertices
-- and a spatial data structure for storing faces.
data Mesh = Mesh (Vector V3) (Octree Face) deriving (Show)

instance IntersectObj Mesh where
  type IntersectionMetaData Mesh = Face
  getIntersect = intersectMeshWithRay


-- Helper function that takes a string "something"
-- and a parser p designed for a line like:
--     "something p p p\n"
-- and pulls out each of the three p values.
--
-- For example, for parseVertex, we have string "v" and floating values
-- parseKeyLine "v" Parse.double takes:
--   """v 1.0 2.0 3.0\n"""
-- and returns [(1.0, 2.0, 3.0)]
parseKeyLine :: Text -> Parse.Parser a -> Parse.Parser (a, a, a)
parseKeyLine key p = do Parse.skipSpace
                        _ <- Parse.string key
                        Parse.skipSpace
                        a <- p
                        Parse.skipSpace
                        b <- p
                        Parse.skipSpace
                        c <- p
                        return (a, b, c)

parseVertex :: Parse.Parser V3
parseVertex = toV3 <$> parseKeyLine "v" Parse.double
  where toV3 (a, b, c) = V a b c

parseFace :: Parse.Parser Face
parseFace = toFace <$> parseKeyLine "f" Parse.decimal
  where toFace (a, b, c) = Face (a-1) (b-1) (c-1)

parseObjLine :: Parse.Parser (Either V3 Face)
parseObjLine = Parse.choice [ Left <$> parseVertex
                            , Right <$> parseFace
                            ]

-- Given a FilePath, open up that file, and try to produce a Mesh from it,
-- assuming it's a valid OBJ file
readInMesh :: FilePath -> IO (Either String Mesh)
readInMesh fp = do fileContents <- TextIO.readFile fp
                   let fileLines = Text.lines fileContents
                       parses    = map (Parse.parseOnly parseObjLine) fileLines
                       goodPs    = Either.rights parses
                       (vs, fs)  = Either.partitionEithers goodPs
                       verts     = Vector.fromList vs
                       octree    = buildOctreeForMesh verts fs
                       mesh      = Mesh verts octree
                   if null goodPs
                   then return $ Left "Bad parse"
                   else return $ Right mesh


-- Reads through all vertices and finds an axis-aligned
-- bounding box that contains them all
computeBoundsForMesh :: Vector V3 -> Bounds
computeBoundsForMesh verts =
    let len         = Vector.length verts
        origin      = V 0 0 0
        emptyBounds = Bounds origin origin

        go i bounds| i < len   =
            let Bounds (V a b c) (V u v w) = bounds
                V x y z                    = Vector.unsafeIndex verts i
                a' = min a x
                b' = min b y
                c' = min c z
                u' = max u x
                v' = max v y
                w' = max w z
            in go (i+1) $ Bounds (V a' b' c') (V u' v' w')
        go _ bounds| otherwise = bounds
    in go 0 emptyBounds


-- O(V + FlogF) Octree construction algorithm
-- where V is the #vertices and F is the #faces
buildOctreeForMesh :: Vector V3 -> [Face] -> Octree Face
buildOctreeForMesh verts faces = buildOctree verts faces (computeBoundsForMesh verts)

intersectMeshWithRay :: Ray -> Mesh -> Maybe (Double, Face)
intersectMeshWithRay r (Mesh verts octreeFaces) =
    let coarseMatches = getOctreeCoarseIntersects octreeFaces r

        -- Now filter the coarse matches for actual hits.
        intersects :: [(Double, Face)]
        intersects = Maybe.mapMaybe (intersectFaceWithRay verts r) coarseMatches

        best = List.minimumBy (comparing fst) intersects

    in if null intersects
       then Nothing
       else Just best

intersectFaceWithRay :: Vector V3 -> Ray -> Face -> Maybe (Double, Face)
intersectFaceWithRay verts r face =
    let (v1, v2, v3) = retrieveVerticesForFace verts face
    in case intersectWithRay (Triangle v1 v2 v3) r of
         Nothing     -> Nothing
         Just (t, _) -> Just (t, face)

retrieveVerticesForFace :: Vector V3 -> Face -> (V3, V3, V3)
retrieveVerticesForFace verts (Face i1 i2 i3) = (verts ! i1, verts ! i2, verts ! i3)
  where (!) = Vector.unsafeIndex
{-# INLINE retrieveVerticesForFace #-}

-- Retrieves the normal vector for a Face in a Mesh.
-- Does not need to be normalized.
getFaceNormal :: Vector V3 -> Face -> V3
getFaceNormal verts face =
    let (v1, v2, v3) = retrieveVerticesForFace verts face
    in (v2 - v1) `cross` (v3 - v2)



-- A triangular mesh is an array of vertices
-- and SLL of Faces
data SlowMesh = SlowMesh (Vector V3) [Face] deriving (Show)

instance IntersectObj SlowMesh where
  type IntersectionMetaData SlowMesh = Face
  getIntersect = intersectSlowMeshWithRay


intersectSlowMeshWithRay :: Ray -> SlowMesh -> Maybe (Double, Face)
intersectSlowMeshWithRay r (SlowMesh verts faces) =
    let coarseMatches = faces

        -- Now filter the coarse matches for actual hits.
        intersects :: [(Double, Face)]
        intersects = Maybe.mapMaybe (intersectFaceWithRay verts r) coarseMatches

        best = List.minimumBy (comparing fst) intersects

    in if null intersects
       then Nothing
       else Just best

-- Given a FilePath, open up that file, and try to produce a Mesh from it,
-- assuming it's a valid OBJ file
readInSlowMesh :: FilePath -> IO (Either String SlowMesh)
readInSlowMesh fp = do fileContents <- TextIO.readFile fp
                       let fileLines = Text.lines fileContents
                           parses    = map (Parse.parseOnly parseObjLine) fileLines
                           goodPs    = Either.rights parses
                           (vs, fs)  = Either.partitionEithers goodPs
                           verts     = Vector.fromList vs
                           mesh      = SlowMesh verts fs
                       if null goodPs
                       then return $ Left "Bad parse"
                       else return $ Right mesh
