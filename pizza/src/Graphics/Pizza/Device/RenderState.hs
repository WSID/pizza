{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}

module Graphics.Pizza.Device.RenderState where

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
import Data.Vector (Vector, (!))

-- vulkan
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import qualified Vulkan.CStruct.Extends as Vk

-- pizza
import Graphics.Pizza.Device.Environment
import Graphics.Pizza.Device.Image
import Graphics.Pizza.Device.Renderer
import Graphics.Pizza.Device.RenderCore
import Graphics.Pizza.Device.RenderTarget
import Graphics.Pizza.Graphic
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

data PreparationImage = PreparationImage {
    patternTransform :: Transform,
    patternOpacity :: Float
}

instance Storable PreparationImage where
    sizeOf _ = 64
    alignment _ = 64

    peek ptr = do
        mx <- peekByteOff ptr 0
        my <- peekByteOff ptr 8
        t <- peekByteOff ptr 16
        opacity <- peekByteOff ptr 32
        pure $ PreparationImage {
            patternTransform = Transform (V2 mx my) t,
            patternOpacity = opacity
        }

    poke ptr PreparationImage {..} = do
            let Transform (V2 mx my) t = patternTransform
            pokeByteOff ptr 0 mx
            pokeByteOff ptr 16 my
            pokeByteOff ptr 32 t
            pokeByteOff ptr 40 patternOpacity



data RenderState = RenderState {
    renderStateCommandBuffer :: Vk.CommandBuffer,
    renderStateVertex :: TypedBuffer (V2 Float),
    renderStateIndex :: TypedBuffer (V3 Word32),
    renderStateScreenDS :: Vk.DescriptorSet,
    renderStatePatternDS :: Vk.DescriptorSet,
    renderStateImageDSs :: Vector Vk.DescriptorSet,
    renderStateScreenUniform :: TypedBuffer (V2 Float),
    renderStatePatternUniform :: TypedBuffer (),
    renderStateTransformUniform :: TypedBuffer (), -- for Transform
    renderStateSemaphore :: Vk.Semaphore,
    renderStateFence :: Vk.Fence
}

data RenderStateSwapchain = RenderStateSwapchain {
    renderStateBase :: RenderState,
    renderStateImageSemaphore :: Vk.Semaphore
}

newRenderState :: (MonadIO m) => RenderCore -> m RenderState
newRenderState RenderCore {..} = do
    let Environment {..} = renderCoreEnvironment
    commandBuffers <- Vk.allocateCommandBuffers
        environmentDevice
        Vk.zero {
            Vk.commandPool = renderCoreCommandPool,
            Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY,
            Vk.commandBufferCount = 1
        }
    let renderStateCommandBuffer = V.head commandBuffers


    renderStateVertex <- newTypedBufferN renderCoreEnvironment
        Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT 4096

    renderStateIndex <- newTypedBufferN renderCoreEnvironment
        Vk.BUFFER_USAGE_INDEX_BUFFER_BIT 4096

    descriptorSets <- Vk.allocateDescriptorSets
        environmentDevice
        Vk.zero {
            Vk.descriptorPool = renderCoreDescriptorPool,
            Vk.setLayouts = V.fromList [ renderCoreScreenDSLayout, renderCorePatternDSLayout]
        }
    
    renderStateImageDSs <- Vk.allocateDescriptorSets
        environmentDevice
        Vk.zero {
            Vk.descriptorPool = renderCoreDescriptorPool,
            Vk.setLayouts = V.replicate 64 renderCorePatternImageDSLayout
        }

    let (renderStateScreenDS, renderStatePatternDS) =
            case V.toList descriptorSets of
                [a, b] -> (a, b)
                _ -> error "newRenderState: Incorrect number of state descriptor sets returned!"

    renderStateScreenUniform <- newTypedBufferN renderCoreEnvironment Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT 1
    renderStatePatternUniform <- newTypedBufferSized renderCoreEnvironment Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT 1024
    renderStateTransformUniform <- newTypedBufferSized renderCoreEnvironment Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT 1024
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

freeRenderState :: (MonadIO m) => RenderCore -> RenderState -> m ()
freeRenderState RenderCore {..} RenderState {..} = do
    let Environment {..} = renderCoreEnvironment
    Vk.destroyFence environmentDevice renderStateFence Nothing
    Vk.destroySemaphore environmentDevice renderStateSemaphore Nothing
    freeTypedBuffer renderCoreEnvironment renderStateScreenUniform
    freeTypedBuffer renderCoreEnvironment renderStatePatternUniform
    freeTypedBuffer renderCoreEnvironment renderStateTransformUniform
    Vk.freeDescriptorSets environmentDevice renderCoreDescriptorPool renderStateImageDSs
    Vk.freeDescriptorSets environmentDevice renderCoreDescriptorPool (V.fromList [renderStateScreenDS, renderStatePatternDS])
    freeTypedBuffer renderCoreEnvironment renderStateIndex
    freeTypedBuffer renderCoreEnvironment renderStateVertex
    Vk.freeCommandBuffers environmentDevice renderCoreCommandPool (V.singleton renderStateCommandBuffer)


newRenderStateSwapchain :: (MonadIO m) => RenderCore -> m RenderStateSwapchain
newRenderStateSwapchain RenderCore {..} = do
    let Environment {..} = renderCoreEnvironment
    renderStateBase <- newRenderState RenderCore {..}
    renderStateImageSemaphore <- Vk.createSemaphore environmentDevice Vk.zero Nothing

    pure RenderStateSwapchain {..}

freeRenderStateSwapchain :: (MonadIO m) => RenderCore -> RenderStateSwapchain -> m ()
freeRenderStateSwapchain RenderCore {..} RenderStateSwapchain {..} = do
    let Environment {..} = renderCoreEnvironment
    Vk.destroySemaphore environmentDevice renderStateImageSemaphore Nothing
    freeRenderState RenderCore {..} renderStateBase


setRenderStateTargetBase :: (MonadIO m) => RenderCore -> Renderer px -> RenderState -> Graphics -> Int -> Int -> BaseRenderTarget px -> m ()
setRenderStateTargetBase RenderCore {..} Renderer {..} RenderState {..} graphics width height BaseRenderTarget {..} = do
    let Environment {..} = renderCoreEnvironment
    let renderArea = Vk.Rect2D {
            Vk.offset = Vk.Offset2D 0 0,
            Vk.extent = Vk.Extent2D (fromIntegral width) (fromIntegral height)
        }

        patOffset = fromIntegral renderCoreMinUniformBufferOffsetAlign

        Graphics drawItems = graphics

    writeTypedBuffer1 renderCoreEnvironment renderStateScreenUniform (fromIntegral <$> V2 width height)

    vptr <- mapTypedBuffer renderCoreEnvironment renderStateVertex
    iptr <- mapTypedBuffer renderCoreEnvironment renderStateIndex
    uptr <- mapTypedBuffer renderCoreEnvironment renderStatePatternUniform
    tptr <- mapTypedBuffer renderCoreEnvironment renderStateTransformUniform

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

    let appendDrawItem :: (Int, Int, Int, Int) -> DrawItem -> IO (Int, Int, Int, Int)
        appendDrawItem (vi, ii, uoff, iindex) (DrawShape paths attrs) = do
            let DrawAttributes {
                    drawPattern = pattern,
                    drawTransform = trans,
                    drawBlend = blend,
                    drawOpacity = opacity
                } = attrs
            (nvi, nii) <- foldlM appendPath (vi, ii) (transform trans <$> paths)

            recordRenderStateStencilCmd
                RenderCore {..}
                Renderer {..}
                RenderState {..}
                renderArea
                (fromIntegral ii)
                (fromIntegral (nii - ii))

            case applyOpacity opacity pattern of
                PatternSolid color -> pokeByteOff uptr uoff color
                PatternLinear ps pe cs ce -> pokeByteOff uptr uoff (PreparationLinear ps pe cs ce)
                PatternRadial ps r cs ce -> pokeByteOff uptr uoff (PreparationRadial ps r cs ce)

                PatternImage image itrans iopacity -> do
                    Vk.updateDescriptorSets
                        environmentDevice
                        ( V.singleton $
                          Vk.SomeStruct $
                          Vk.WriteDescriptorSet {
                            Vk.next = (),
                            Vk.dstSet = renderStateImageDSs ! iindex,
                            Vk.dstBinding = 0,
                            Vk.dstArrayElement = 0,
                            Vk.descriptorCount = 1,
                            Vk.descriptorType = Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                            Vk.bufferInfo = V.empty,
                            Vk.imageInfo = V.singleton Vk.DescriptorImageInfo {
                                Vk.sampler = imageSampler image,
                                Vk.imageView = imageView image,
                                Vk.imageLayout = Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
                            },
                            Vk.texelBufferView = V.empty
                          }
                        )
                        V.empty

                    pokeByteOff uptr uoff (PreparationImage itrans iopacity)

            let Transform (V2 transMx transMy) transT = trans

            pokeByteOff tptr uoff transMx
            pokeByteOff tptr (uoff + 16) transMy
            pokeByteOff tptr (uoff + 32) transT

            recordRenderStateColorCmd
                RenderCore {..}
                Renderer {..}
                RenderState {..}
                pattern
                blend
                (fromIntegral uoff)
                iindex
            
            let niindex = case pattern of
                    PatternImage {} -> iindex + 1
                    _ -> iindex

            pure (nvi, nii, uoff + patOffset, niindex)

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
    _ <- liftIO $ foldlM appendDrawItem (0, 0, 0, 0) drawItems

    Vk.cmdEndRenderPass renderStateCommandBuffer

    Vk.endCommandBuffer renderStateCommandBuffer

    unmapTypedBuffer renderCoreEnvironment renderStateTransformUniform
    unmapTypedBuffer renderCoreEnvironment renderStatePatternUniform
    unmapTypedBuffer renderCoreEnvironment renderStateIndex
    unmapTypedBuffer renderCoreEnvironment renderStateVertex

recordRenderStateStencilCmd :: (MonadIO m) => RenderCore -> Renderer px -> RenderState -> Vk.Rect2D -> Word32 -> Word32 -> m ()
recordRenderStateStencilCmd RenderCore {..} Renderer {..} RenderState {..} renderArea indexStart indexCount = do
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
        renderCoreStencilPipelineLayout
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

recordRenderStateColorCmd :: (MonadIO m) => RenderCore -> Renderer px -> RenderState -> Pattern -> Blend -> Word32 -> Int -> m ()
recordRenderStateColorCmd RenderCore {..} Renderer {..} RenderState {..} pattern blend patOffset imageIndex = do
    Vk.cmdBindDescriptorSets
        renderStateCommandBuffer
        Vk.PIPELINE_BIND_POINT_GRAPHICS
        renderCorePatternLayout
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
            PatternImage {} -> rendererPatternImage
        )

    Vk.cmdBindIndexBuffer
        renderStateCommandBuffer
        (typedBufferObject renderCoreQuadIndices) 0 Vk.INDEX_TYPE_UINT32

    Vk.cmdBindVertexBuffers
        renderStateCommandBuffer
        0
        (V.singleton $ typedBufferObject renderCoreQuadVertices)
        (V.singleton 0)

    Vk.cmdBindDescriptorSets
        renderStateCommandBuffer
        Vk.PIPELINE_BIND_POINT_GRAPHICS
        renderCorePatternLayout
        1
        (V.singleton renderStatePatternDS)
        (V.fromList [patOffset, patOffset])

    case pattern of
        PatternImage _ _ _ -> do
            Vk.cmdBindDescriptorSets
                renderStateCommandBuffer
                Vk.PIPELINE_BIND_POINT_GRAPHICS
                renderCorePatternLayout
                2
                (V.singleton $ renderStateImageDSs ! imageIndex)
                V.empty
        
        _ -> pure () 

    
    Vk.cmdSetColorBlendAdvancedEXT
        renderStateCommandBuffer
        0
        (V.singleton $ blendToBlendState blend)

    Vk.cmdDrawIndexed renderStateCommandBuffer 6 1 0 0 0

renderRenderStateTargetBase :: (MonadIO m) => RenderCore -> Renderer px -> RenderState -> Graphics -> BaseRenderTarget px -> Vk.Extent2D -> Maybe Vk.Semaphore -> m (m ())
renderRenderStateTargetBase RenderCore {..} Renderer {..} RenderState {..} graphics BaseRenderTarget {..} size wait = do
    let Environment {..} = renderCoreEnvironment
    let Vk.Extent2D {
        Vk.width = width,
        Vk.height = height
    } = size

    setRenderStateTargetBase
        RenderCore {..}
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

renderRenderStateTarget :: (MonadIO m) => RenderCore -> Renderer px -> RenderState -> Graphics -> RenderTarget px -> Maybe Vk.Semaphore -> m (m ())
renderRenderStateTarget RenderCore {..} Renderer {..} RenderState {..} graphics RenderTarget {..} =
    renderRenderStateTargetBase RenderCore {..} Renderer {..} RenderState {..} graphics renderTargetBase renderTargetSize

renderRenderStateTargetSurface :: (MonadIO m) => RenderCore -> Renderer px -> RenderStateSwapchain -> Graphics -> SurfaceRenderTarget px -> m (Int, m ())
renderRenderStateTargetSurface RenderCore {..} Renderer {..} RenderStateSwapchain {..} graphics SurfaceRenderTarget {..} = do
    let Environment {..} = renderCoreEnvironment
        RenderState {..} = renderStateBase

    (_, indexw) <- Vk.acquireNextImageKHR
        environmentDevice
        renderTargetSwapchain
        maxBound -- timeout in nanosecs
        renderStateImageSemaphore
        Vk.NULL_HANDLE -- fence
    let index = fromIntegral indexw

    waitOp <- renderRenderStateTargetBase
        RenderCore {..}
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