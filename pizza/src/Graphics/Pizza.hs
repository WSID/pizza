{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}

module Graphics.Pizza (
    module Graphics.Pizza.Preparation,
    module Graphics.Pizza.Renderer,
    module Graphics.Pizza.RenderTarget,
    render
) where

import Control.Monad.IO.Class

import Data.Bits

import Foreign.Storable
import Foreign.Ptr

-- linear
import Linear

-- vector
import qualified Data.Vector as V
import Data.Vector (Vector)

-- vulkan
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import qualified Vulkan.CStruct.Extends as Vk

-- VulkanMemoryAllocator
import qualified VulkanMemoryAllocator as Vma


-- pizza
import Graphics.Pizza.Preparation
import Graphics.Pizza.Renderer
import Graphics.Pizza.RenderTarget


render :: (MonadIO m) => Environment -> Maybe Vk.Semaphore -> Vector Vk.Semaphore -> Vk.Fence -> Renderer -> Preparation -> Int -> Int ->  BaseRenderTarget -> m ()
render Environment {..} wait signal fence Renderer {..} Preparation {..} width height BaseRenderTarget {..} = do
    recordBaseRenderTarget
        preparationCommandBuffer
        Renderer {..}
        width height
        BaseRenderTarget {..}
        $ recordPreparation preparationCommandBuffer Renderer {..} Preparation {..}

    uniformPtr <- Vma.mapMemory environmentAllocator preparationBufferUniformAlloc

    liftIO $ poke (castPtr uniformPtr) (V2 (fromIntegral width) (fromIntegral height) :: V2 Float)
    Vma.unmapMemory environmentAllocator preparationBufferUniformAlloc

    case wait of
        Just w -> Vk.queueSubmit environmentGraphicsQueue
            (V.singleton $ Vk.SomeStruct Vk.zero {
                Vk.waitSemaphores = V.singleton w,
                Vk.waitDstStageMask = V.singleton Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
                Vk.commandBuffers = V.singleton $ Vk.commandBufferHandle preparationCommandBuffer,
                Vk.signalSemaphores = signal
            } )
            fence
        Nothing -> Vk.queueSubmit environmentGraphicsQueue
            (V.singleton $ Vk.SomeStruct Vk.zero {
                Vk.waitSemaphores = V.empty,
                Vk.waitDstStageMask = V.empty,
                Vk.commandBuffers = V.singleton $ Vk.commandBufferHandle preparationCommandBuffer,
                Vk.signalSemaphores = signal
            } )
            fence


