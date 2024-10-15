{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}

module Graphics.Pizza.RenderState where

import Control.Monad.IO.Class

import Data.Functor
import Data.Bits

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
import Graphics.Pizza.Preparation
import Graphics.Pizza.Renderer
import Graphics.Pizza.RenderTarget
import Graphics.Pizza.Internal.TypedBuffer

-- Render State!

data RenderState = RenderState {
    renderStateCommandBuffer :: Vk.CommandBuffer,
    renderStateScreenDS :: Vk.DescriptorSet,
    renderStateUniform :: TypedBuffer (V2 Float),
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

    descriptorSets <- Vk.allocateDescriptorSets
        environmentDevice
        Vk.zero {
            Vk.descriptorPool = rendererDescriptorPool,
            Vk.setLayouts = V.singleton rendererScreenDSLayout
        }
    let renderStateScreenDS = V.head descriptorSets

    renderStateUniform <- newTypedBufferN Renderer {..} Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT 1
    renderStateSemaphore <- Vk.createSemaphore environmentDevice Vk.zero Nothing
    renderStateFence <- Vk.createFence environmentDevice Vk.zero Nothing

    Vk.updateDescriptorSets
        environmentDevice
        -- writes
        (V.singleton $ Vk.SomeStruct Vk.zero {
            Vk.next = (),
            Vk.dstSet = renderStateScreenDS,
            Vk.dstBinding = 0,
            Vk.dstArrayElement = 0,
            Vk.descriptorCount = 1,
            Vk.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER,
            Vk.bufferInfo = V.singleton $ Vk.zero {
                Vk.buffer = typedBufferObject renderStateUniform,
                Vk.offset = 0,
                Vk.range = Vk.WHOLE_SIZE
            }
        } )
        -- copies
        V.empty

    pure RenderState {..}

freeRenderState :: (MonadIO m) => Renderer -> RenderState -> m ()
freeRenderState Renderer {..} RenderState {..} = do
    let Environment {..} = rendererEnvironment
    Vk.destroyFence environmentDevice renderStateFence Nothing
    Vk.destroySemaphore environmentDevice renderStateSemaphore Nothing
    freeTypedBuffer Renderer {..} renderStateUniform
    Vk.freeDescriptorSets environmentDevice rendererDescriptorPool (V.singleton renderStateScreenDS)
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


setRenderStateTargetBase :: (MonadIO m) => Renderer -> RenderState -> Preparation -> Int -> Int -> BaseRenderTarget -> m ()
setRenderStateTargetBase Renderer {..} RenderState {..} Preparation {..} width height BaseRenderTarget {..} = do
    let renderArea = Vk.Rect2D {
        Vk.offset = Vk.Offset2D 0 0,
        Vk.extent = Vk.Extent2D (fromIntegral width) (fromIntegral height)
    }

    writeTypedBuffer1 Renderer {..} renderStateUniform (fromIntegral <$> V2 width height)

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

            recordPreparationCommand Renderer {..} Preparation {..} renderStateCommandBuffer

renderRenderStateTarget :: (MonadIO m) => Renderer -> RenderState -> Preparation -> RenderTarget -> Maybe Vk.Semaphore -> m (m ())
renderRenderStateTarget Renderer {..} RenderState {..} Preparation {..} RenderTarget {..} wait = do
    let Environment {..} = rendererEnvironment
    let Vk.Extent2D {
        Vk.width = width,
        Vk.height = height
    } = renderTargetSize

    setRenderStateTargetBase
        Renderer {..}
        RenderState {..}
        Preparation {..}
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

renderRenderStateTargetSwapchain :: (MonadIO m) => Renderer -> RenderStateSwapchain -> Preparation -> SwapchainRenderTarget -> m (Int, m ())
renderRenderStateTargetSwapchain Renderer {..} RenderStateSwapchain {..} Preparation {..} SwapchainRenderTarget {..} = do
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
        Preparation {..}
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
