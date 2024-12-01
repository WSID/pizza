{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}

module Graphics.Pizza.RenderState where

import Control.Monad.IO.Class

import Data.Functor
import Data.Bits
import Data.Word

import Foreign.Ptr
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
    renderStateSemaphore :: Vk.Semaphore,
    renderStateFence :: Vk.Fence
}

data RenderStateSwapchain = RenderStateSwapchain {
    renderStateBase :: RenderState,
    renderStateImageSemaphore :: Vk.Semaphore
}

newRenderState :: (MonadIO m) => Renderer -> m RenderState
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
        Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT 1024

    renderStateIndex <- newTypedBufferN rendererEnvironment
        Vk.BUFFER_USAGE_INDEX_BUFFER_BIT 1024

    descriptorSets <- Vk.allocateDescriptorSets
        environmentDevice
        Vk.zero {
            Vk.descriptorPool = rendererDescriptorPool,
            Vk.setLayouts = V.fromList [ rendererScreenDSLayout, rendererPatternDSLayout ]
        }
    let [renderStateScreenDS, renderStatePatternDS] = V.toList descriptorSets

    renderStateScreenUniform <- newTypedBufferN rendererEnvironment Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT 1
    renderStatePatternUniform <- newTypedBufferSized rendererEnvironment Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT 64
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
                Vk.bufferInfo = V.singleton $ Vk.zero {
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
                Vk.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                Vk.bufferInfo = V.singleton $ Vk.zero {
                    Vk.buffer = typedBufferObject renderStatePatternUniform,
                    Vk.offset = 0,
                    Vk.range = Vk.WHOLE_SIZE
                }
            }
        ])
        -- copies
        V.empty

    pure RenderState {..}

freeRenderState :: (MonadIO m) => Renderer -> RenderState -> m ()
freeRenderState Renderer {..} RenderState {..} = do
    let Environment {..} = rendererEnvironment
    Vk.destroyFence environmentDevice renderStateFence Nothing
    Vk.destroySemaphore environmentDevice renderStateSemaphore Nothing
    freeTypedBuffer rendererEnvironment renderStateScreenUniform
    freeTypedBuffer rendererEnvironment renderStatePatternUniform
    Vk.freeDescriptorSets environmentDevice rendererDescriptorPool (V.fromList [renderStateScreenDS, renderStatePatternDS])
    freeTypedBuffer rendererEnvironment renderStateIndex
    freeTypedBuffer rendererEnvironment renderStateVertex
    Vk.freeCommandBuffers environmentDevice rendererCommandPool (V.singleton renderStateCommandBuffer)


newRenderStateSwapchain :: (MonadIO m) => Renderer -> m RenderStateSwapchain
newRenderStateSwapchain Renderer {..} = do
    let Environment {..} = rendererEnvironment
    renderStateBase <- newRenderState Renderer {..}
    renderStateImageSemaphore <- Vk.createSemaphore environmentDevice Vk.zero Nothing

    pure RenderStateSwapchain {..}

freeRenderStateSwapchain :: (MonadIO m) => Renderer -> RenderStateSwapchain -> m ()
freeRenderStateSwapchain Renderer {..} RenderStateSwapchain {..} = do
    let Environment {..} = rendererEnvironment
    Vk.destroySemaphore environmentDevice renderStateImageSemaphore Nothing
    freeRenderState Renderer {..} renderStateBase


setRenderStateTargetBase :: (MonadIO m) => Renderer -> RenderState -> Graphics -> Int -> Int -> BaseRenderTarget -> m ()
setRenderStateTargetBase Renderer {..} RenderState {..} graphics width height BaseRenderTarget {..} = do
    let Graphics path pattern = graphics
    let renderArea = Vk.Rect2D {
        Vk.offset = Vk.Offset2D 0 0,
        Vk.extent = Vk.Extent2D (fromIntegral width) (fromIntegral height)
    }

    -- Vertex

    let Path vertices _ = path

    vptr <- mapTypedBuffer rendererEnvironment renderStateVertex
    liftIO $ pokeArray vptr vertices
    unmapTypedBuffer rendererEnvironment renderStateVertex

    -- Indices

    let (indicesFirst: indicesSnd: indicesRest) = zipWith const [0 .. ] vertices
    let indices = zipWith (\a b -> V3 indicesFirst a b) (indicesSnd: indicesRest) indicesRest

    iptr <- mapTypedBuffer rendererEnvironment renderStateIndex
    liftIO $ pokeArray iptr indices
    unmapTypedBuffer rendererEnvironment renderStateIndex

    -- Pattern Uniforms

    writeTypedBuffer1 rendererEnvironment  renderStateScreenUniform (fromIntegral <$> V2 width height)
    ptr <- mapTypedBuffer rendererEnvironment renderStatePatternUniform
    liftIO $ case pattern of
        PatternSolid color -> poke (castPtr ptr) color
        PatternLinear ps pe cs ce -> poke (castPtr ptr) (PreparationLinear ps pe cs ce)
        PatternRadial ps r cs ce -> poke (castPtr ptr) (PreparationRadial ps r cs ce)
    unmapTypedBuffer rendererEnvironment renderStatePatternUniform

    Vk.resetCommandBuffer renderStateCommandBuffer zeroBits
    Vk.useCommandBuffer
        renderStateCommandBuffer
        Vk.zero {
            Vk.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
        }
        $ do
        Vk.cmdSetViewport renderStateCommandBuffer 0 $ V.singleton Vk.Viewport {
            Vk.x = 0,
            Vk.y = 0,
            Vk.width = fromIntegral width,
            Vk.height = fromIntegral height,
            Vk.minDepth = 0,
            Vk.maxDepth = 1
        }

        Vk.cmdSetScissor renderStateCommandBuffer 0 $ V.singleton renderArea

        Vk.cmdUseRenderPass
            renderStateCommandBuffer
            Vk.zero {
                Vk.renderPass = rendererRenderPass,
                Vk.framebuffer = renderTargetFramebuffer,
                Vk.renderArea = renderArea,
                Vk.clearValues = V.singleton (Vk.Color $ Vk.Float32 0 0 0 1)
            }
            Vk.SUBPASS_CONTENTS_INLINE
            $ do

            Vk.cmdBindDescriptorSets
                renderStateCommandBuffer
                Vk.PIPELINE_BIND_POINT_GRAPHICS
                rendererPatternLayout
                0
                (V.singleton renderStateScreenDS)
                (V.empty)

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
                (typedBufferObject renderStateIndex) 0 Vk.INDEX_TYPE_UINT32

            Vk.cmdBindVertexBuffers
                renderStateCommandBuffer
                0
                (V.singleton $ typedBufferObject renderStateVertex)
                (V.singleton 0)

            Vk.cmdBindDescriptorSets
                renderStateCommandBuffer
                Vk.PIPELINE_BIND_POINT_GRAPHICS
                rendererPatternLayout
                1
                (V.singleton renderStatePatternDS)
                (V.empty)

            Vk.cmdDrawIndexed renderStateCommandBuffer (3 * (fromIntegral $ length indices)) 1 0 0 0

renderRenderStateTarget :: (MonadIO m) => Renderer -> RenderState -> Graphics -> RenderTarget -> Maybe Vk.Semaphore -> m (m ())
renderRenderStateTarget Renderer {..} RenderState {..} graphics RenderTarget {..} wait = do
    let Environment {..} = rendererEnvironment
    let Vk.Extent2D {
        Vk.width = width,
        Vk.height = height
    } = renderTargetSize

    setRenderStateTargetBase
        Renderer {..}
        RenderState {..}
        graphics
        (fromIntegral width)
        (fromIntegral height)
        renderTargetBase

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

renderRenderStateTargetSwapchain :: (MonadIO m) => Renderer -> RenderStateSwapchain -> Graphics -> SwapchainRenderTarget -> m (Int, m ())
renderRenderStateTargetSwapchain Renderer {..} RenderStateSwapchain {..} graphics SwapchainRenderTarget {..} = do
    let Environment {..} = rendererEnvironment
        RenderState {..} = renderStateBase
    let Vk.Extent2D {
        Vk.width = width,
        Vk.height = height
    } = renderTargetSize

    (_, indexw) <- Vk.acquireNextImageKHR
        environmentDevice
        renderTargetSwapchain
        maxBound -- timeout in nanosecs
        renderStateImageSemaphore
        Vk.NULL_HANDLE -- fence

    let index = fromIntegral indexw

    setRenderStateTargetBase
        Renderer {..}
        RenderState {..}
        graphics
        (fromIntegral width) (fromIntegral height)
        (renderTargetBase ! index)

    Vk.resetFences environmentDevice (V.singleton renderStateFence)

    Vk.queueSubmit environmentGraphicsQueue
        (V.singleton $ Vk.SomeStruct Vk.zero {
            Vk.waitSemaphores = V.singleton renderStateImageSemaphore,
            Vk.waitDstStageMask = V.singleton Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
            Vk.commandBuffers = V.singleton $ Vk.commandBufferHandle renderStateCommandBuffer,
            Vk.signalSemaphores = V.singleton renderStateSemaphore
        } )
        renderStateFence

    _ <- Vk.queuePresentKHR
        environmentGraphicsQueue
        Vk.zero {
            Vk.waitSemaphores = V.singleton renderStateSemaphore,
            Vk.swapchains = V.singleton renderTargetSwapchain,
            Vk.imageIndices = V.singleton indexw
        }

    let waitOp = void $ Vk.waitForFences environmentDevice (V.singleton renderStateFence) True maxBound

    pure $ (index, waitOp)
