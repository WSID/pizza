{-# LANGUAGE DuplicateRecordFields #-}

module Demo where

import Data.Bits
import Data.Foldable
import Data.Word

import Linear

import qualified Data.Vector as V

import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import qualified Vulkan.CStruct.Extends as Vk

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
                Pz.localDashPattern (const (50 + 50 * sin theta : cycle [50, 50, 50, 50])) $ do
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

    let environment = Pz.renderCoreEnvironment renderCore

    -- Command Buffer
    cmdpool <- Vk.createCommandPool
        (Pz.environmentDevice environment)
        Vk.CommandPoolCreateInfo {
            Vk.flags = Vk.COMMAND_POOL_CREATE_TRANSIENT_BIT,
            Vk.queueFamilyIndex = Pz.environmentGraphicsQFI environment
        }
        Nothing

    cmdbufs <- Vk.allocateCommandBuffers
        (Pz.environmentDevice environment)
        Vk.CommandBufferAllocateInfo {
            Vk.commandPool = cmdpool,
            Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY,
            Vk.commandBufferCount = 1
        }

    let cmdbuf = case V.toList cmdbufs of
            [cmd] -> cmd
            _ -> error "Unexpected count of command buffers"

    fence <- Vk.createFence
        (Pz.environmentDevice environment)
        Vk.FenceCreateInfo {
            Vk.next = (),
            Vk.flags = zeroBits
        }
        Nothing

    staging <- Pz.newTypedBufferN environment Vk.BUFFER_USAGE_TRANSFER_SRC_BIT (width * height) :: IO (Pz.TypedBuffer (Pz.VRGBA (Pz.UNorm Word8)))
    image <- Pz.newImage (Pz.renderCoreEnvironment renderCore) width height
    Pz.writeTypedBufferN environment staging (fmap mapper coordinates)

    Vk.useCommandBuffer
        cmdbuf
        Vk.CommandBufferBeginInfo {
            Vk.next = (),
            Vk.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
            Vk.inheritanceInfo = Nothing
        } $ do
            -- Transit image layout so that we can copy exchange content to image.
            Vk.cmdPipelineBarrier cmdbuf
                Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT
                Vk.PIPELINE_STAGE_TRANSFER_BIT
                zeroBits
                V.empty
                V.empty
                (V.singleton $ Vk.SomeStruct Vk.ImageMemoryBarrier {
                    Vk.next = (),
                    Vk.srcAccessMask = Vk.ACCESS_NONE,
                    Vk.dstAccessMask = Vk.ACCESS_TRANSFER_WRITE_BIT,
                    Vk.oldLayout = Vk.IMAGE_LAYOUT_UNDEFINED,
                    Vk.newLayout = Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                    Vk.srcQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED,
                    Vk.dstQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED,
                    Vk.image = Pz.imageObject image,
                    Vk.subresourceRange = Vk.ImageSubresourceRange {
                        Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT,
                        Vk.baseMipLevel = 0,
                        Vk.levelCount = 1,
                        Vk.baseArrayLayer = 0,
                        Vk.layerCount = 1
                    }
                })

            -- Copy exchange content to image.
            _ <- Vk.cmdCopyBufferToImage cmdbuf
                (Pz.typedBufferObject staging)
                (Pz.imageObject image)
                Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
                (V.singleton Vk.BufferImageCopy {
                    Vk.bufferOffset = 0,
                    Vk.bufferRowLength = 0,
                    Vk.bufferImageHeight = 0,
                    Vk.imageSubresource = Vk.ImageSubresourceLayers {
                        Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT,
                        Vk.mipLevel = 0,
                        Vk.baseArrayLayer = 0,
                        Vk.layerCount = 1
                    },
                    Vk.imageOffset = Vk.Offset3D 0 0 0,
                    Vk.imageExtent = Vk.Extent3D (fromIntegral width) (fromIntegral height) 1
                })

            -- Transfer image layout so that it can be sampled from shader.
            Vk.cmdPipelineBarrier cmdbuf
                Vk.PIPELINE_STAGE_TRANSFER_BIT
                Vk.PIPELINE_STAGE_FRAGMENT_SHADER_BIT
                zeroBits
                V.empty
                V.empty
                (V.singleton $ Vk.SomeStruct Vk.ImageMemoryBarrier {
                    Vk.next = (),
                    Vk.srcAccessMask = Vk.ACCESS_TRANSFER_WRITE_BIT,
                    Vk.dstAccessMask = Vk.ACCESS_SHADER_READ_BIT,
                    Vk.oldLayout = Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                    Vk.newLayout = Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                    Vk.srcQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED,
                    Vk.dstQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED,
                    Vk.image = Pz.imageObject image,
                    Vk.subresourceRange = Vk.ImageSubresourceRange {
                        Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT,
                        Vk.baseMipLevel = 0,
                        Vk.levelCount = 1,
                        Vk.baseArrayLayer = 0,
                        Vk.layerCount = 1
                    }
                })

    Vk.queueSubmit
        (Pz.environmentGraphicsQueue environment)
        (V.singleton $ Vk.SomeStruct Vk.zero {
            Vk.commandBuffers = V.singleton $ Vk.commandBufferHandle cmdbuf
        } )
        fence

    _ <- Vk.waitForFences
        (Pz.environmentDevice environment)
        (V.singleton fence)
        True
        maxBound

    Pz.freeTypedBuffer environment staging
    Vk.destroyFence (Pz.environmentDevice environment) fence Nothing
    Vk.destroyCommandPool (Pz.environmentDevice environment) cmdpool Nothing
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
            Pz.localDashPattern (const $ cycle [40, 20]) $ do
                Pz.paintStrokeOpen road

    -- TODO: Left, Right side of path.
    -- TODO: Objects moving on road.