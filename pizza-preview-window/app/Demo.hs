module Demo where

import Control.Monad

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