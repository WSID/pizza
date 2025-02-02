{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Pizza.Device.Exchange where

import Control.Monad.IO.Class

import Data.Bits

import Foreign.Ptr
import Foreign.Storable

import qualified Data.Vector as V

import qualified Vulkan as Vk
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Zero as Vk

import Graphics.Pizza.Device.Environment
import Graphics.Pizza.Device.Renderer
import Graphics.Pizza.Internal.TypedBuffer
import Graphics.Pizza.Device.RenderTarget

data Exchange a = Exchange {
    exchangeCommandBuffer :: Vk.CommandBuffer,
    exchangeBuffer :: TypedBuffer a,
    exchangeFence :: Vk.Fence
}

newExchangeSized :: (MonadIO m) => Renderer px -> Int -> m (Exchange a)
newExchangeSized renderer size = do
    let Environment {..} = rendererEnvironment renderer

    commandBuffers <- Vk.allocateCommandBuffers
        environmentDevice 
        Vk.CommandBufferAllocateInfo {
            Vk.commandPool = rendererCommandPool renderer,
            Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY,
            Vk.commandBufferCount = 1
        }
    
    let exchangeCommandBuffer = case V.toList commandBuffers of
            [a] -> a
            _ -> error "Exchange Command Buffer Count not match."
    
    exchangeBuffer <- newTypedBufferSized
        Environment {..}
        (Vk.BUFFER_USAGE_TRANSFER_SRC_BIT .|. Vk.BUFFER_USAGE_TRANSFER_DST_BIT)
        size
    
    exchangeFence <- Vk.createFence environmentDevice Vk.zero Nothing
    
    pure $ Exchange {..}


newExchangeNA :: (MonadIO m, Storable a) => Renderer px -> Int -> a -> m (Exchange a)
newExchangeNA renderer n a = newExchangeSized renderer (n * sizeOf a)

newExchangeN :: (MonadIO m, Storable a) => Renderer px -> Int -> m (Exchange a)
newExchangeN renderer n = newExchangeNA renderer n undefined

castExchange :: Exchange a -> Exchange b
castExchange exchange = exchange {
    exchangeBuffer = castTypedBuffer $ exchangeBuffer exchange
}

freeExchange :: (MonadIO m) => Renderer px -> Exchange a -> m ()
freeExchange renderer exchange = do
    let Environment {..} = rendererEnvironment renderer

    freeTypedBuffer Environment {..} (exchangeBuffer exchange)

    Vk.freeCommandBuffers
        environmentDevice
        (rendererCommandPool renderer)
        (V.singleton $ exchangeCommandBuffer exchange)


mapExchange :: (MonadIO m) => Renderer px -> Exchange a -> m (Ptr a)
mapExchange renderer exchange = mapTypedBuffer (rendererEnvironment renderer) (exchangeBuffer exchange)

unmapExchange :: (MonadIO m) => Renderer px -> Exchange a -> m ()
unmapExchange renderer exchange = unmapTypedBuffer (rendererEnvironment renderer) (exchangeBuffer exchange)

writeExchange1 :: (MonadIO m, Storable a) => Renderer px -> Exchange a -> a -> m ()
writeExchange1 renderer exchange = writeTypedBuffer1 (rendererEnvironment renderer) (exchangeBuffer exchange)

readExchange1 :: (MonadIO m, Storable a) => Renderer px -> Exchange a -> m a
readExchange1 renderer exchange = readTypedBuffer1 (rendererEnvironment renderer) (exchangeBuffer exchange)

writeExchangeN :: (MonadIO m, Storable a) => Renderer px -> Exchange a -> [a] -> m ()
writeExchangeN renderer exchange = writeTypedBufferN (rendererEnvironment renderer) (exchangeBuffer exchange)

readExchangeN :: (MonadIO m, Storable a) => Renderer px -> Exchange a -> Int -> m [a]
readExchangeN renderer exchange = readTypedBufferN (rendererEnvironment renderer) (exchangeBuffer exchange)

-- This now requires a command buffer write, and wait.
-- TODO: How to wrap semaphore?
writeExchangeRenderTarget :: (MonadIO m) => Renderer px -> Exchange a -> RenderTarget a -> Maybe Vk.Semaphore -> m ()
writeExchangeRenderTarget renderer Exchange {..} RenderTarget {..} maySem = do
    let Environment {..} = rendererEnvironment renderer
    let Vk.Extent2D width height  = renderTargetSize

    Vk.useCommandBuffer exchangeCommandBuffer Vk.CommandBufferBeginInfo {
        Vk.next = (),
        Vk.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
        Vk.inheritanceInfo = Nothing
    } $ do
        Vk.cmdCopyImageToBuffer exchangeCommandBuffer
            renderTargetImage
            Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
            (typedBufferObject exchangeBuffer)
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
                Vk.imageExtent = Vk.Extent3D width height 1
            })
    
    -- TODO: Wait for transfer

    let (sems, masks) = case maySem of
            Nothing -> (V.empty, V.empty)
            Just sem -> (V.singleton sem, V.singleton Vk.PIPELINE_STAGE_TRANSFER_BIT)

    Vk.queueSubmit environmentGraphicsQueue
        (V.singleton $ Vk.SomeStruct Vk.SubmitInfo {
            Vk.next = (),
            Vk.waitSemaphores = sems,
            Vk.waitDstStageMask = masks,
            Vk.commandBuffers = V.singleton $ Vk.commandBufferHandle exchangeCommandBuffer,
            Vk.signalSemaphores = V.empty
        })
        exchangeFence
