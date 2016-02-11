{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Ray.RayGen where

import Ray.Cast
import Ray.Geometry

import Control.Monad
import Control.Monad.Random

import qualified Data.List as List
import qualified Data.List.Split as Split
import System.Random.Shuffle (shuffleM)


-- Retrieves the 3D position of the view plane cell center
viewPlaneCellCenter :: (Int, Int) -> Camera -> V3
viewPlaneCellCenter (c,r) Camera{..} =
    let wFunc i res = let i'      = fromIntegral i
                          halfRes = fromIntegral res / 2.0
                      in cellLength * (i' - halfRes + 0.5)
        xw = wFunc c width
        yw = wFunc r height

        scaledRight = (lookAt `cross` up) `scaledTo` xw
        scaledUp    = up `scaledTo` yw

        viewPlaneCenter = position + (lookAt `scaledTo` focalLength)

    in viewPlaneCenter + scaledRight + scaledUp

-- Simulates perspective by generating rays that go from the
-- focal point of the camera to the view plane.
perspectiveRayGen :: RayGenFunc
perspectiveRayGen cellCoords cam =
    let cellCenter = viewPlaneCellCenter cellCoords cam

        eyePoint = position cam

        rayDir = cellCenter - eyePoint

    in [ray eyePoint rayDir]


-- Simulates orthographic projection by generating rays normal
-- to the view plane.
orthographicRayGen :: RayGenFunc
orthographicRayGen cellCoords cam =
    let cellCenter = viewPlaneCellCenter cellCoords cam

        parallelPoint = cellCenter - rayDir

        rayDir = lookAt cam

    in [ray parallelPoint rayDir]



-- Shoots w*w samples per pixel
multiJitteredRayGen :: Int -> RayGenFunc
multiJitteredRayGen w (c,r) Camera{..} =
    let range = [0 .. w-1]

        -- Canonical arrangement
        canon = Split.chunksOf w [(w-1-r', w-1-c') | c' <- range, r' <- range]

        -- Process to turn canonical arrangement into jittered
        jittering :: Rand StdGen [[(Int, Int)]]
        jittering = do -- First shuffle the X coords in each column
                      xShuffled <- mapM shuffleM canon
                      -- Now shuffle the Y coords in each row
                      let rows = List.transpose xShuffled
                      yShuffled <- forM rows $ \row -> do
                                       let (xs, ys) = List.unzip row
                                       ys' <- shuffleM ys
                                       return $ zip xs ys'
                      return $ List.transpose yShuffled

        -- The local, fine grid coordinates
        jittered :: [[(Int, Int)]]
        jittered = evalRand jittering (mkStdGen $ 7 * c + r)

        -- Connects each large grid coordinate with the fine grid coordinate
        jitteredWithCoords :: [( (Int,Int), (Int,Int) )]
        jitteredWithCoords = zip [(c',r') | c' <- range, r' <- range] $ concat jittered

       -- Convert each of these into a Ray
    in flip map jitteredWithCoords $ \((cc, cr), (fc, fr)) ->

           let coarseGridLength = cellLength / fromIntegral w
               fineGridLength   = coarseGridLength / fromIntegral w

               jitterFunc i l res = let i'      = fromIntegral i
                                        halfRes =  fromIntegral res / 2.0
                                    in l * (i' - halfRes + 0.5)

               fLocalC = jitterFunc fc fineGridLength w
               fLocalR = jitterFunc fr fineGridLength w

               cLocalC = jitterFunc cc coarseGridLength w
               cLocalR = jitterFunc cr coarseGridLength w

               pLocalC = jitterFunc c cellLength width
               pLocalR = jitterFunc r cellLength height

               xw = fLocalC + cLocalC + pLocalC
               yw = fLocalR + cLocalR + pLocalR


               scaledRight = (lookAt `cross` up) `scaledTo` xw
               scaledUp    = up `scaledTo` yw

               viewPlaneCenter = position + (lookAt `scaledTo` focalLength)

               jitterPoint = viewPlaneCenter + scaledRight + scaledUp

           in ray position (jitterPoint-position)
