module Ray.AreaLightShape where

import Ray.Geometry
import Ray.Shape
import Ray.Mesh
import qualified Control.Monad.Random as Random
import qualified Data.Vector          as Vector
import Control.Monad (replicateM)

data AreaLightShape = Rect3D {
                        origin :: V3
                      , uv :: V3
                      , vv :: V3
                      }
                    deriving (Show)


-- Specify the number of sample points desired
getSamplePoints :: Int -> AreaLightShape -> [V3]
getSamplePoints k (Rect3D v1 e1 e2) =
    let seed = Random.mkStdGen (round $ sqrMagnitude v1)

        randUV :: Random.MonadRandom m => m (Double, Double)
        randUV = (,) <$> Random.getRandomR (0,1) <*> Random.getRandomR (0,1)

        uvs :: [(Double, Double)]
        uvs = flip Random.evalRand seed $ replicateM k randUV

        computePoint (u,v) = v1 + (u .* e1) + (v .* e2)

    in map computePoint uvs


-- #419begin
--   #type=1
--   #src=http://stackoverflow.com/questions/18663755/how-to-convert-a-3d-point-on-a-plane-to-uv-coordinates
-- #419end
areaDistanceSquared :: AreaLightShape -> V3 -> Double
areaDistanceSquared (Rect3D v1 e1 e2) p =
    let u = e1 .*. p
        v = e2 .*. p

        u' = max 0 $ min u $ magnitude e1
        v' = max 0 $ min v $ magnitude e2

        pOnRect = v1 + (u' .* e1) + (v' .* e2)

    in sqrMagnitude (p - pOnRect)

computeShapeOrMesh :: AreaLightShape -> Either Shape Mesh
computeShapeOrMesh (Rect3D v1 e1 e2) =
    let v2 = v1 + e1
        v3 = v1 + e1 + e2
        v4 = v1 + e2

        verts = Vector.fromList [v1, v2, v3, v4]
        f1 = Face 0 1 3
        f2 = Face 1 2 3

    in Right $ Mesh verts $ buildOctreeForMesh verts [f1, f2]
