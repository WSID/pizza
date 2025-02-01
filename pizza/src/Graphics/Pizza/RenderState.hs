{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}

module Graphics.Pizza.RenderState where

import Control.Monad
import Control.Monad.IO.Class

import Data.Bits
import Data.Foldable
import Data.Word

import Foreign.Storable
import Foreign.Marshal.Array

-- linear
import Linear

-- vector
import qualified Data.Vector as V
import Data.Vector ((!))

-- vulkan
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import qualified Vulkan.CStruct.Extends as Vk

-- pizza
import Graphics.Pizza.Environment
import Graphics.Pizza.Graphic
import Graphics.Pizza.Renderer
import Graphics.Pizza.RenderTarget
import Graphics.Pizza.Internal.TypedBuffer

-- Render State!


data PreparationLinear = PreparationLinear {
    patternPosStart :: V2 Float,
    patternPosEnd :: V2 Float,
    patternColorStart :: V4 Float,
    patternColorEnd :: V4 Float
}

instance Storable PreparationLinear where
    sizeOf _ = 48
    alignment _ = 8

    peek ptr = do
        patternPosStart <- peekByteOff ptr 0
        patternPosEnd <- peekByteOff ptr 8
        patternColorStart <- peekByteOff ptr 16
        patternColorEnd <- peekByteOff ptr 32
        pure PreparationLinear {..}

    poke ptr PreparationLinear {..} = do
        pokeByteOff ptr 0 patternPosStart
        pokeByteOff ptr 8 patternPosEnd
        pokeByteOff ptr 16 patternColorStart
        pokeByteOff ptr 32 patternColorEnd


data PreparationRadial = PreparationRadial {
    patternPosCenter :: V2 Float,
    patternPosRadius :: Float,
    patternColorStart :: V4 Float,
    patternColorEnd :: V4 Float
}

instance Storable PreparationRadial where
    sizeOf _ = 48
    alignment _ = 8

    peek ptr = do
        patternPosCenter <- peekByteOff ptr 0
        patternPosRadius <- peekByteOff ptr 8
        patternColorStart <- peekByteOff ptr 16
        patternColorEnd <- peekByteOff ptr 32
        pure PreparationRadial {..}

    poke ptr PreparationRadial {..} = do
        pokeByteOff ptr 0 patternPosCenter
        pokeByteOff ptr 8 patternPosRadius
        pokeByteOff ptr 16 patternColorStart
        pokeByteOff ptr 32 patternColorEnd

data RenderState = RenderState {
    renderStateCommandBuffer :: Vk.CommandBuffer,
    renderStateVertex :: TypedBuffer (V2 Float),
    renderStateIndex :: TypedBuffer (V3 Word32),
    renderStateScreenDS :: Vk.DescriptorSet,
    renderStateScreenUniform :: TypedBuffer (V2 Float),
    renderStatePatternDS :: Vk.DescriptorSet,
    renderStatePatternUniform :: TypedBuffer (),
    renderStateTransformUniform :: TypedBuffer (), -- for Transform
    renderStateSemaphore :: Vk.Semaphore,
    renderStateFence :: Vk.Fence
}

data RenderStateSwapchain = RenderStateSwapchain {
    renderStateBase :: RenderState,
    renderStateImageSemaphore :: Vk.Semaphore
}

newRenderState :: (MonadIO m) => Renderer px -> m RenderState
newRenderState Renderer {..} = do
    let Environment {..} = rendererEnvironment
    commandBuffers <- Vk.allocateCommandBuffers
        environmentDevice
        Vk.zero {
            Vk.commandPool = rendererCommandPool,
            Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY,
            Vk.commandBufferCount = 1
        }
    let renderStateCommandBuffer = V.head commandBuffers


    renderStateVertex <- newTypedBufferN rendererEnvironment
        Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT 4096

    renderStateIndex <- newTypedBufferN rendererEnvironment
        Vk.BUFFER_USAGE_INDEX_BUFFER_BIT 4096

    descriptorSets <- Vk.allocateDescriptorSets
        environmentDevice
        Vk.zero {
            Vk.descriptorPool = rendererDescriptorPool,
            Vk.setLayouts = V.fromList [ rendererScreenDSLayout, rendererPatternDSLayout ]
        }

    let (renderStateScreenDS, renderStatePatternDS) =
            case V.toList descriptorSets of
                [a, b] -> (a, b)
                _ -> error "newRenderState: Incorrect number of state descriptor sets returned!"

    renderStateScreenUniform <- newTypedBufferN rendererEnvironment Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT 1
    renderStatePatternUniform <- newTypedBufferSized rendererEnvironment Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT 1024
    renderStateTransformUniform <- newTypedBufferSized rendererEnvironment Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT 1024
    renderStateSemaphore <- Vk.createSemaphore environmentDevice Vk.zero Nothing
    renderStateFence <- Vk.createFence environmentDevice Vk.zero Nothing

    Vk.updateDescriptorSets
        environmentDevice
        -- writes
        (V.fromList [
            Vk.SomeStruct Vk.zero {
                Vk.next = (),
                Vk.dstSet = renderStateScreenDS,
                Vk.dstBinding = 0,
                Vk.dstArrayElement = 0,
                Vk.descriptorCount = 1,
                Vk.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                Vk.bufferInfo = V.singleton Vk.DescriptorBufferInfo {
                    Vk.buffer = typedBufferObject renderStateScreenUniform,
                    Vk.offset = 0,
                    Vk.range = Vk.WHOLE_SIZE
                }
            },
            Vk.SomeStruct Vk.zero {
                Vk.next = (),
                Vk.dstSet = renderStatePatternDS,
                Vk.dstBinding = 0,
                Vk.dstArrayElement = 0,
                Vk.descriptorCount = 1,
                Vk.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC,
                Vk.bufferInfo = V.singleton Vk.DescriptorBufferInfo {
                    Vk.buffer = typedBufferObject renderStatePatternUniform,
                    Vk.offset = 0,
                    Vk.range = 64
                }
            },
            Vk.SomeStruct Vk.zero {
                Vk.next = (),
                Vk.dstSet = renderStatePatternDS,
                Vk.dstBinding = 1,
                Vk.dstArrayElement = 0,
                Vk.descriptorCount = 1,
                Vk.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC,
                Vk.bufferInfo = V.singleton Vk.DescriptorBufferInfo {
                    Vk.buffer = typedBufferObject renderStateTransformUniform,
                    Vk.offset = 0,
                    Vk.range = 64
                }
            }
        ])
        -- copies
        V.empty

    pure RenderState {..}

freeRenderState :: (MonadIO m) => Renderer px -> RenderState -> m ()
freeRenderState Renderer {..} RenderState {..} = do
    let Environment {..} = rendererEnvironment
    Vk.destroyFence environmentDevice renderStateFence Nothing
    Vk.destroySemaphore environmentDevice renderStateSemaphore Nothing
    freeTypedBuffer rendererEnvironment renderStateScreenUniform
    freeTypedBuffer rendererEnvironment renderStatePatternUniform
    freeTypedBuffer rendererEnvironment renderStateTransformUniform
    Vk.freeDescriptorSets environmentDevice rendererDescriptorPool (V.fromList [renderStateScreenDS, renderStatePatternDS])
    freeTypedBuffer rendererEnvironment renderStateIndex
    freeTypedBuffer rendererEnvironment renderStateVertex
    Vk.freeCommandBuffers environmentDevice rendererCommandPool (V.singleton renderStateCommandBuffer)


newRenderStateSwapchain :: (MonadIO m) => Renderer px -> m RenderStateSwapchain
newRenderStateSwapchain Renderer {..} = do
    let Environment {..} = rendererEnvironment
    renderStateBase <- newRenderState Renderer {..}
    renderStateImageSemaphore <- Vk.createSemaphore environmentDevice Vk.zero Nothing

    pure RenderStateSwapchain {..}

freeRenderStateSwapchain :: (MonadIO m) => Renderer px -> RenderStateSwapchain -> m ()
freeRenderStateSwapchain Renderer {..} RenderStateSwapchain {..} = do
    let Environment {..} = rendererEnvironment
    Vk.destroySemaphore environmentDevice renderStateImageSemaphore Nothing
    freeRenderState Renderer {..} renderStateBase


setRenderStateTargetBase :: (MonadIO m) => Renderer px -> RenderState -> Graphics -> Int -> Int -> BaseRenderTarget px -> m ()
setRenderStateTargetBase Renderer {..} RenderState {..} graphics width height BaseRenderTarget {..} = do
    let renderArea = Vk.Rect2D {
            Vk.offset = Vk.Offset2D 0 0,
            Vk.extent = Vk.Extent2D (fromIntegral width) (fromIntegral height)
        }

        patOffset = fromIntegral rendererMinUniformBufferOffsetAlign

        Graphics drawItems = graphics

    writeTypedBuffer1 rendererEnvironment renderStateScreenUniform (fromIntegral <$> V2 width height)

    vptr <- mapTypedBuffer rendererEnvironment renderStateVertex
    iptr <- mapTypedBuffer rendererEnvironment renderStateIndex
    uptr <- mapTypedBuffer rendererEnvironment renderStatePatternUniform
    tptr <- mapTypedBuffer rendererEnvironment renderStateTransformUniform

    Vk.resetCommandBuffer renderStateCommandBuffer zeroBits

    let appendPath :: (Int, Int) -> Path -> IO (Int, Int)
        appendPath (pathVi, pathIi) path = do
            let pathVertices = pathToPoints path
                pathVcount = length pathVertices
                pathViend = pathVi + pathVcount

                pathVirange = [fromIntegral pathVi .. fromIntegral (pred pathViend)]
                pathIndices = fanIndices pathVirange

            pokeArray (advancePtr vptr pathVi) pathVertices
            pokeArray (advancePtr iptr pathIi) pathIndices
            pure (pathViend, pathIi + pathVcount - 2)

    let appendDrawItem :: (Int, Int, Int) -> DrawItem -> IO (Int, Int, Int)
        appendDrawItem (vi, ii, uoff) (DrawShape paths attrs) = do
            let DrawAttributes {
                    drawPattern = pattern,
                    drawTransform = trans,
                    drawBlend = blend,
                    drawOpacity = opacity
                } = attrs
            (nvi, nii) <- foldlM appendPath (vi, ii) (transform trans <$> paths)

            recordRenderStateStencilCmd
                Renderer {..}
                RenderState {..}
                renderArea
                (fromIntegral ii)
                (fromIntegral (nii - ii))

            case applyOpacity opacity pattern of
                PatternSolid color -> pokeByteOff uptr uoff color
                PatternLinear ps pe cs ce -> pokeByteOff uptr uoff (PreparationLinear ps pe cs ce)
                PatternRadial ps r cs ce -> pokeByteOff uptr uoff (PreparationRadial ps r cs ce)

            let Transform (V2 transMx transMy) transT = trans

            pokeByteOff tptr uoff transMx
            pokeByteOff tptr (uoff + 16) transMy
            pokeByteOff tptr (uoff + 32) transT

            recordRenderStateColorCmd
                Renderer {..}
                RenderState {..}
                pattern
                blend
                (fromIntegral uoff)

            pure (nvi, nii, uoff + patOffset)

    Vk.beginCommandBuffer
        renderStateCommandBuffer
        Vk.CommandBufferBeginInfo {
            Vk.next = (),
            Vk.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
            Vk.inheritanceInfo = Nothing
        }

    Vk.cmdSetViewport renderStateCommandBuffer 0 $ V.singleton Vk.Viewport {
        Vk.x = 0,
        Vk.y = 0,
        Vk.width = fromIntegral width,
        Vk.height = fromIntegral height,
        Vk.minDepth = 0,
        Vk.maxDepth = 1
    }

    Vk.cmdSetScissor renderStateCommandBuffer 0 $ V.singleton renderArea

    Vk.cmdBeginRenderPass
        renderStateCommandBuffer
        Vk.zero {
            Vk.renderPass = rendererRenderPass,
            Vk.framebuffer = renderTargetFramebuffer,
            Vk.renderArea = renderArea,
            Vk.clearValues = V.fromList [
                    Vk.Color (Vk.Float32 0 0 0 1),
                    Vk.DepthStencil (Vk.ClearDepthStencilValue 0 0)
                ]
        }
        Vk.SUBPASS_CONTENTS_INLINE

    -- This might be useful performance data.
    -- (Represents vertices count and triangle counts)
    _ <- liftIO $ foldlM appendDrawItem (0, 0, 0) drawItems

    Vk.cmdEndRenderPass renderStateCommandBuffer

    Vk.endCommandBuffer renderStateCommandBuffer

    unmapTypedBuffer rendererEnvironment renderStateTransformUniform
    unmapTypedBuffer rendererEnvironment renderStatePatternUniform
    unmapTypedBuffer rendererEnvironment renderStateIndex
    unmapTypedBuffer rendererEnvironment renderStateVertex

recordRenderStateStencilCmd :: (MonadIO m) => Renderer px -> RenderState -> Vk.Rect2D -> Word32 -> Word32 -> m ()
recordRenderStateStencilCmd Renderer {..} RenderState {..} renderArea indexStart indexCount = do
    Vk.cmdClearAttachments
        renderStateCommandBuffer
        (V.singleton Vk.ClearAttachment {
            Vk.aspectMask = Vk.IMAGE_ASPECT_STENCIL_BIT,
            Vk.colorAttachment = 0,
            Vk.clearValue = Vk.DepthStencil (Vk.ClearDepthStencilValue 0 0)
        })
        (V.singleton Vk.ClearRect {
            Vk.rect = renderArea,
            Vk.baseArrayLayer = 0,
            Vk.layerCount = 1
        })

    Vk.cmdBindDescriptorSets
        renderStateCommandBuffer
        Vk.PIPELINE_BIND_POINT_GRAPHICS
        rendererStencilPipelineLayout
        0
        (V.singleton renderStateScreenDS)
        V.empty

    Vk.cmdBindPipeline
        renderStateCommandBuffer
        Vk.PIPELINE_BIND_POINT_GRAPHICS
        rendererStencilPipeline

    Vk.cmdBindIndexBuffer
        renderStateCommandBuffer
        (typedBufferObject renderStateIndex) 0 Vk.INDEX_TYPE_UINT32

    Vk.cmdBindVertexBuffers
        renderStateCommandBuffer
        0
        (V.singleton $ typedBufferObject renderStateVertex)
        (V.singleton 0)

    Vk.cmdDrawIndexed renderStateCommandBuffer (3 * indexCount) 1 (3 * indexStart) 0 0

recordRenderStateColorCmd :: (MonadIO m) => Renderer px -> RenderState -> Pattern -> Blend -> Word32 -> m ()
recordRenderStateColorCmd Renderer {..} RenderState {..} pattern blend patOffset = do
    Vk.cmdBindDescriptorSets
        renderStateCommandBuffer
        Vk.PIPELINE_BIND_POINT_GRAPHICS
        rendererPatternLayout
        0
        (V.singleton renderStateScreenDS)
        V.empty

    Vk.cmdBindPipeline
        renderStateCommandBuffer
        Vk.PIPELINE_BIND_POINT_GRAPHICS
        (case pattern of
            PatternSolid _ -> rendererPatternSolid
            PatternLinear {} -> rendererPatternLinear
            PatternRadial {} -> rendererPatternRadial
        )

    Vk.cmdBindIndexBuffer
        renderStateCommandBuffer
        (typedBufferObject rendererQuadIndices) 0 Vk.INDEX_TYPE_UINT32

    Vk.cmdBindVertexBuffers
        renderStateCommandBuffer
        0
        (V.singleton $ typedBufferObject rendererQuadVertices)
        (V.singleton 0)

    Vk.cmdBindDescriptorSets
        renderStateCommandBuffer
        Vk.PIPELINE_BIND_POINT_GRAPHICS
        rendererPatternLayout
        1
        (V.singleton renderStatePatternDS)
        (V.fromList [patOffset, patOffset])
    
    Vk.cmdSetColorBlendAdvancedEXT
        renderStateCommandBuffer
        0
        (V.singleton $ blendToBlendState blend)

    Vk.cmdDrawIndexed renderStateCommandBuffer 6 1 0 0 0

renderRenderStateTargetBase :: (MonadIO m) => Renderer px -> RenderState -> Graphics -> BaseRenderTarget px -> Vk.Extent2D -> Maybe Vk.Semaphore -> m (m ())
renderRenderStateTargetBase Renderer {..} RenderState {..} graphics BaseRenderTarget {..} size wait = do
    let Environment {..} = rendererEnvironment
    let Vk.Extent2D {
        Vk.width = width,
        Vk.height = height
    } = size

    setRenderStateTargetBase
        Renderer {..}
        RenderState {..}
        graphics
        (fromIntegral width)
        (fromIntegral height)
        BaseRenderTarget {..}

    Vk.resetFences environmentDevice (V.singleton renderStateFence)

    let (waitSemaphores, waitDstStageMask) = case wait of
            Nothing -> (V.empty, V.empty)
            Just v -> (V.singleton v, V.singleton Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)

    Vk.queueSubmit environmentGraphicsQueue
        (V.singleton $ Vk.SomeStruct Vk.zero {
            Vk.waitSemaphores = waitSemaphores,
            Vk.waitDstStageMask = waitDstStageMask,
            Vk.commandBuffers = V.singleton $ Vk.commandBufferHandle renderStateCommandBuffer,
            Vk.signalSemaphores = V.singleton renderStateSemaphore
        } )
        renderStateFence

    pure $ void $ Vk.waitForFences environmentDevice (V.singleton renderStateFence) True maxBound

renderRenderStateTarget :: (MonadIO m) => Renderer px -> RenderState -> Graphics -> RenderTarget px -> Maybe Vk.Semaphore -> m (m ())
renderRenderStateTarget Renderer {..} RenderState {..} graphics RenderTarget {..} =
    renderRenderStateTargetBase Renderer {..} RenderState {..} graphics renderTargetBase renderTargetSize

renderRenderStateTargetSwapchain :: (MonadIO m) => Renderer px -> RenderStateSwapchain -> Graphics -> SwapchainRenderTarget px -> m (Int, m ())
renderRenderStateTargetSwapchain Renderer {..} RenderStateSwapchain {..} graphics SwapchainRenderTarget {..} = do
    let Environment {..} = rendererEnvironment
        RenderState {..} = renderStateBase

    (_, indexw) <- Vk.acquireNextImageKHR
        environmentDevice
        renderTargetSwapchain
        maxBound -- timeout in nanosecs
        renderStateImageSemaphore
        Vk.NULL_HANDLE -- fence

    let index = fromIntegral indexw

    waitOp <- renderRenderStateTargetBase
        Renderer {..}
        renderStateBase
        graphics
        (renderTargetBase ! index)
        renderTargetSize
        (Just renderStateImageSemaphore)

    _ <- Vk.queuePresentKHR
        environmentGraphicsQueue
        Vk.zero {
            Vk.waitSemaphores = V.singleton renderStateSemaphore,
            Vk.swapchains = V.singleton renderTargetSwapchain,
            Vk.imageIndices = V.singleton indexw
        }

    pure (index, waitOp)


fanIndices :: [a] -> [V3 a]
fanIndices (f: s: r) = zipWith (V3 f) (s : r) r
fanIndices _ = []

blendToBlendState :: Blend -> Vk.ColorBlendAdvancedEXT
blendToBlendState blend = Vk.ColorBlendAdvancedEXT {
    Vk.advancedBlendOp = blendToBlendOp blend,
    Vk.srcPremultiplied = False,
    Vk.dstPremultiplied = False,
    Vk.blendOverlap = Vk.BLEND_OVERLAP_UNCORRELATED_EXT,
    Vk.clampResults = False
}

blendToBlendOp :: Blend -> Vk.BlendOp
blendToBlendOp BlendNormal = Vk.BLEND_OP_SRC_OVER_EXT
blendToBlendOp BlendMultiply = Vk.BLEND_OP_MULTIPLY_EXT
blendToBlendOp BlendScreen = Vk.BLEND_OP_SCREEN_EXT
blendToBlendOp BlendOverlay = Vk.BLEND_OP_OVERLAY_EXT
blendToBlendOp BlendDarken = Vk.BLEND_OP_DARKEN_EXT
blendToBlendOp BlendLighten = Vk.BLEND_OP_LIGHTEN_EXT
blendToBlendOp BlendColorDodge = Vk.BLEND_OP_COLORDODGE_EXT
blendToBlendOp BlendColorBurn = Vk.BLEND_OP_COLORBURN_EXT
blendToBlendOp BlendHardLight = Vk.BLEND_OP_HARDLIGHT_EXT
blendToBlendOp BlendSoftLight = Vk.BLEND_OP_SOFTLIGHT_EXT
blendToBlendOp BlendDifference = Vk.BLEND_OP_DIFFERENCE_EXT
blendToBlendOp BlendExclusion = Vk.BLEND_OP_EXCLUSION_EXT
blendToBlendOp BlendHue = Vk.BLEND_OP_HSL_HUE_EXT
blendToBlendOp BlendSaturation = Vk.BLEND_OP_HSL_SATURATION_EXT
blendToBlendOp BlendColor = Vk.BLEND_OP_HSL_COLOR_EXT
blendToBlendOp BlendLuminosity = Vk.BLEND_OP_HSL_LUMINOSITY_EXT