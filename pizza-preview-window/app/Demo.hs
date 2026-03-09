module Demo where

import Control.Monad

import Data.Foldable
import Data.Word

import Linear

import qualified Graphics.Pizza as Pz

data Demo = Demo
    { demoGraphic :: Float -> Pz.Graphics
    , demoFree :: IO ()
    }

makeBasicDemo :: Pz.RenderCore -> Int -> Int -> IO Demo
makeBasicDemo renderCore width height = do
    image <- makeWaveImage renderCore width height
    pure Demo
        { demoGraphic = makeBasicGraphic image
        , demoFree = Pz.freeImage (Pz.renderCoreEnvironment renderCore) image
        }


makeBasicGraphic :: Pz.Image px -> Float -> Pz.Graphics
makeBasicGraphic image time = let env = Pz.defPaintingEnv { Pz.paintingThickness = 10 }
    in flip Pz.runPainting_ env $ do
        let theta = time * 2
            animValue1 = 400 * cos theta
            animValue2 = 100 * sin theta

        -- Shape 1
        let pattern1 = Pz.PatternImage
                image
                (Pz.fromScale (V2 0.01 0.01))
                1

        Pz.localPattern (const pattern1) $ do
            Pz.localOpacity (const (abs (sin theta))) $ do
                Pz.paintFillShaping $ do
                    Pz.shapingPathing $ do
                        Pz.pathingCurve $ Pz.bezier
                            (V2 0 200)
                            [ V2 0 (0 - animValue1), V2 400 (400 + animValue1) ]
                            (V2 400 200)

                        Pz.pathingCurve $ Pz.arc
                            (V2 (300 + animValue2) 200)
                            (100 - animValue2)
                            0
                            (negate pi)

                        Pz.pathingCurve $ Pz.arc
                            (V2 (100 + animValue2) 200)
                            (100 + animValue2)
                            0
                            pi
                    Pz.shapingPath () $ Pz.circle (V2 200 200) (100 + animValue2)

        -- Shape 2
        let pattern2 = Pz.PatternLinear
                (V2 (200 - 200 * cos theta) (200 + 200 * sin theta))
                (V2 (200 + 200 * cos theta) (200 - 200 * sin theta))
                (V4 0 1 0 1)
                (V4 1 0 1 1)

        Pz.localTransform (Pz.scale (pure (abs (cos theta)))) $ do
            Pz.localPattern (const pattern2) $ do
                Pz.localEndCaps (const (Pz.strokeCapRound, Pz.strokeCapRound)) $ do
                    Pz.localBlend (const Pz.BlendOverlay) $ do
                        Pz.paintStrokeOpenPathing $ do
                            Pz.pathingCurve $ Pz.arc (V2 300 100) 50 (pi * (-0.5)) (pi * (0.5))
                            Pz.pathingCurve $ Pz.arc (V2 100 100) 50 (pi * (0.5)) (pi * (1.5))

        -- Shape 3
        let pattern3 = Pz.PatternSolid (V4 0.5 0.5 0.5 1)

        Pz.localTransform (Pz.translate (V2 0 200)) $ do
            Pz.localPattern (const pattern3) $ do
                Pz.localDashPattern (const $ Just (Pz.DashPattern True (50 + 50 * sin theta : cycle [50, 50, 50, 50]))) $ do
                    Pz.localBlend (const Pz.BlendLighten) $ do
                        Pz.paintStrokeClosePathing $ do
                            Pz.pathingCurve $ Pz.arc (V2 300 100) 50 (pi * (-0.5)) (pi * (0.5))
                            Pz.pathingCurve $ Pz.arc (V2 100 100) 50 (pi * (0.5)) (pi * (1.5))


makeWaveImage :: Pz.RenderCore -> Int -> Int -> IO (Pz.Image (Pz.VRGBA (Pz.UNorm Word8)))
makeWaveImage renderCore width height = do
    let coordinates = do
            y <- [0, 1 .. width]
            x <- [0, 1 .. height]
            pure $ V2 x y

        fw = fromIntegral width :: Float
        fh = fromIntegral height :: Float

        mapper (V2 x y) = Pz.VRGBA $ V4 (Pz.UNorm b) (Pz.UNorm g) (Pz.UNorm r) (Pz.UNorm 255)
          where
            fx = fromIntegral x :: Float
            fy = fromIntegral y :: Float
            sp = (1 + sin (fx * pi * 2 / fw)) * 0.5
            cp = (1 + cos (fy * pi * 2 / fh)) * 0.5
            b = round (sp * 255)
            g = round (sqrt (sp * cp) * 255)
            r = round (cp * 255)

    exchange <- Pz.newExchangeN renderCore (width * height)
    image <- Pz.newImage (Pz.renderCoreEnvironment renderCore) width height
    Pz.writeExchangeN renderCore exchange (fmap mapper coordinates)
    join $ Pz.copyExchangeToImage renderCore exchange image Nothing

    Pz.freeExchange renderCore exchange
    pure image



-- Curve demo

-- | A Pizza Demo that shows graph plotting.
--
-- Shows off custom curve feature.
--
makeGraphDemo :: IO Demo
makeGraphDemo = pure $ Demo
    { demoGraphic = makeGraphGraphics
    , demoFree = pure ()
    }

makeGraphGraphics :: Float -> Pz.Graphics
makeGraphGraphics _time = let env = Pz.defPaintingEnv { Pz.paintingThickness = 10 }
    in flip Pz.runPainting_ env $ do

        Pz.localThickness (const 2) $ do

            -- Paint Grid
            Pz.localPattern (const $ Pz.PatternSolid (V4 0.5 0.5 0.5 1.0)) $ do
                -- X grid
                for_ ([200, 200 - 50 .. 0] <> [200, 200 + 50 .. 400]) $ \x -> do
                    Pz.paintStrokeOpen (Pz.polygon [V2 x 0, V2 x 400])
                -- Y grid
                for_ ([200, 200 - 50 .. 0] <> [200, 200 + 50 .. 400]) $ \y -> do
                    Pz.paintStrokeOpen (Pz.polygon [V2 0 y, V2 400 y])


            -- Paint Graph

            Pz.localPattern (const $ Pz.PatternSolid (V4 1 1 1 1)) $ do
                Pz.paintStrokeOpen (Pz.Path [Pz.PathPoint (V2 0 400), Pz.PathPoint (V2 400 0)])

            let g1 x = x * x    -- square

            Pz.localPattern (const $ Pz.PatternSolid (V4 1.0  0.25  0.25  1.0)) $ do
                Pz.paintStrokeOpen (Pz.Path [Pz.PathCurve $ makeGraphCurve g1])

            let g2 x = sin x    -- sin

            Pz.localPattern (const $ Pz.PatternSolid (V4 0.25  1.0  0.25  1.0)) $ do
                Pz.paintStrokeOpen (Pz.Path [Pz.PathCurve $ makeGraphCurve g2])

            let g3 x = cos x    -- sin

            Pz.localPattern (const $ Pz.PatternSolid (V4 1.0  0.25  1.0  1.0)) $ do
                Pz.paintStrokeOpen (Pz.Path [Pz.PathCurve $ makeGraphCurve g3])

            let g4 x = exp x    -- exp

            Pz.localPattern (const $ Pz.PatternSolid (V4 0.25  0.25  1.0  1.0)) $ do
                Pz.paintStrokeOpen (Pz.Path [Pz.PathCurve $ makeGraphCurve g4])

            let g5 x = recip (1 + exp (negate x))

            Pz.localPattern (const $ Pz.PatternSolid (V4 0.25  1.0  1.0  1.0)) $ do
                Pz.paintStrokeOpen (Pz.Path [Pz.PathCurve $ makeGraphCurve g5])


-- | Make curve from function.
--
-- `makeGraphCurve func` will be a curve that plots `func x` where `x in [-4 .. 4]`.
--
-- The plot is remapped from [V2 (-4) (-4) .. V2 4 4] to [V2 0 0 .. V2 400 400].
-- Also flipped upside down as pizza and math plotting has opposite Y direction.
--
makeGraphCurve :: (Float -> Float) -> Pz.Curve
makeGraphCurve func = Pz.Curve
    { Pz.curvePosition = \a ->
        let fx = (a * 400 - 200) / 50
        in V2 (400 * a) (200 - (func fx * 50))
    , Pz.curveDirection = \a ->
        let fx = (a * 400 - 200) / 50
        in normalize $ V2 0.02 (func (fx - 0.01) - func (fx + 0.01))
    }


-- Outline Demo


-- | A Pizza Demo that shows a village with road ways.
--
-- This shows off outline features.
makeRoadDemo :: IO Demo
makeRoadDemo = pure $ Demo
    { demoGraphic = makeRoadGraphics
    , demoFree = pure ()
    }

makeRoadGraphics :: Float -> Pz.Graphics
makeRoadGraphics _time = flip Pz.runPainting_ Pz.defPaintingEnv $ do
    -- Draw Landscape
    Pz.localPattern (const $ Pz.PatternSolid (V4 0.6 1.0 0.5 1.0)) $ do
        Pz.paintFillShaping $ do
            Pz.shapingPathing $ do
                Pz.pathingPoint $ V2 0 0
                Pz.pathingPoint $ V2 400 0
                Pz.pathingPoint $ V2 400 400
                Pz.pathingPoint $ V2 0 400

    let Pz.Pathing _ road = do
            Pz.pathingCurve $ Pz.arc (V2 0 200) 100 (0.5 * pi) (0)
            Pz.pathingCurve $ Pz.bezier
                (V2 100 200)
                [ V2 100 0
                , V2 300 50
                , V2 300 300
                ]
                (V2 400 300)

    -- The road itself
    Pz.localPattern (const $ Pz.PatternSolid (V4 0.6 0.6 0.6 1.0)) $ do
        Pz.localThickness (const 50) $ do
            Pz.paintStrokeOpen road

    -- Center lin
    Pz.localPattern (const $ Pz.PatternSolid (V4 1 1 1 1)) $ do
        Pz.localThickness (const 2) $ do
            Pz.localDashPattern (const $ Just (Pz.DashPattern True (cycle [40, 20]))) $ do
                Pz.paintStrokeOpen road

    -- TODO: Left, Right side of path.
    -- TODO: Objects moving on road.