{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Pizza.Device.Exchange where

import Control.Monad.IO.Class
import Control.Monad
import Data.Bits
import Foreign.Ptr
import Foreign.Storable

import qualified Data.Vector as V

import qualified Vulkan as Vk
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Zero as Vk

import Graphics.Pizza.Device.Environment
import Graphics.Pizza.Device.RenderCore
import Graphics.Pizza.Internal.TypedBuffer
import Graphics.Pizza.Device.RenderTarget
import Graphics.Pizza.Device.Image

data Exchange a = Exchange {
    exchangeCommandBuffer :: Vk.CommandBuffer,
    exchangeBuffer :: TypedBuffer a,
    exchangeFence :: Vk.Fence
}

newExchangeSized :: (MonadIO m) => RenderCore -> Int -> m (Exchange a)
newExchangeSized renderCore size = do
    let Environment {..} = renderCoreEnvironment renderCore

    commandBuffers <- Vk.allocateCommandBuffers
        environmentDevice 
        Vk.CommandBufferAllocateInfo {
            Vk.commandPool = renderCoreCommandPool renderCore,
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


newExchangeNA :: (MonadIO m, Storable a) => RenderCore -> Int -> a -> m (Exchange a)
newExchangeNA renderCore n a = newExchangeSized renderCore (n * sizeOf a)

newExchangeN :: (MonadIO m, Storable a) => RenderCore -> Int -> m (Exchange a)
newExchangeN renderCore n = newExchangeNA renderCore n undefined

castExchange :: Exchange a -> Exchange b
castExchange exchange = exchange {
    exchangeBuffer = castTypedBuffer $ exchangeBuffer exchange
}

freeExchange :: (MonadIO m) => RenderCore -> Exchange a -> m ()
freeExchange renderCore exchange = do
    let Environment {..} = renderCoreEnvironment renderCore

    Vk.destroyFence
        environmentDevice
        (exchangeFence exchange)
        Nothing

    freeTypedBuffer Environment {..} (exchangeBuffer exchange)

    Vk.freeCommandBuffers
        environmentDevice
        (renderCoreCommandPool renderCore)
        (V.singleton $ exchangeCommandBuffer exchange)



mapExchange :: (MonadIO m) => RenderCore -> Exchange a -> m (Ptr a)
mapExchange renderCore exchange = mapTypedBuffer (renderCoreEnvironment renderCore) (exchangeBuffer exchange)

unmapExchange :: (MonadIO m) => RenderCore -> Exchange a -> m ()
unmapExchange renderCore exchange = unmapTypedBuffer (renderCoreEnvironment renderCore) (exchangeBuffer exchange)

writeExchange1 :: (MonadIO m, Storable a) => RenderCore -> Exchange a -> a -> m ()
writeExchange1 renderCore exchange = writeTypedBuffer1 (renderCoreEnvironment renderCore) (exchangeBuffer exchange)

readExchange1 :: (MonadIO m, Storable a) => RenderCore -> Exchange a -> m a
readExchange1 renderCore exchange = readTypedBuffer1 (renderCoreEnvironment renderCore) (exchangeBuffer exchange)

writeExchangeN :: (MonadIO m, Storable a) => RenderCore -> Exchange a -> [a] -> m ()
writeExchangeN renderCore exchange = writeTypedBufferN (renderCoreEnvironment renderCore) (exchangeBuffer exchange)

readExchangeN :: (MonadIO m, Storable a) => RenderCore -> Exchange a -> Int -> m [a]
readExchangeN renderCore exchange = readTypedBufferN (renderCoreEnvironment renderCore) (exchangeBuffer exchange)

-- This now requires a command buffer write, and wait.
-- TODO: How to wrap semaphore?
writeExchangeRenderTarget :: (MonadIO m) => RenderCore -> Exchange a -> RenderTarget a -> Maybe Vk.Semaphore -> m (m ())
writeExchangeRenderTarget renderCore Exchange {..} RenderTarget {..} maySem = do
    let Environment {..} = renderCoreEnvironment renderCore
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
    
    pure (void $ Vk.waitForFences environmentDevice (V.singleton exchangeFence) True maxBound)

copyExchangeToImage :: (MonadIO m) => RenderCore -> Exchange a -> Image a -> Maybe Vk.Semaphore -> m (m ())
copyExchangeToImage renderCore Exchange {..} Image {..} maySem = do

    let Environment {..} = renderCoreEnvironment renderCore
    let Vk.Extent2D width height  = imageSize

    Vk.useCommandBuffer exchangeCommandBuffer Vk.CommandBufferBeginInfo {
        Vk.next = (),
        Vk.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
        Vk.inheritanceInfo = Nothing
    } $ do

        -- Transit image layout so that we can copy exchange content to image.
        Vk.cmdPipelineBarrier exchangeCommandBuffer
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
                Vk.image = imageObject,
                Vk.subresourceRange = Vk.ImageSubresourceRange {
                    Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT,
                    Vk.baseMipLevel = 0,
                    Vk.levelCount = 1,
                    Vk.baseArrayLayer = 0,
                    Vk.layerCount = 1
                }
            })

        -- Copy exchange content to image.
        _ <- Vk.cmdCopyBufferToImage exchangeCommandBuffer
            (typedBufferObject exchangeBuffer)
            imageObject
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
                Vk.imageExtent = Vk.Extent3D width height 1
            })

        -- Transfer image layout so that it can be sampled from shader.
        Vk.cmdPipelineBarrier exchangeCommandBuffer
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
                Vk.image = imageObject,
                Vk.subresourceRange = Vk.ImageSubresourceRange {
                    Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT,
                    Vk.baseMipLevel = 0,
                    Vk.levelCount = 1,
                    Vk.baseArrayLayer = 0,
                    Vk.layerCount = 1
                }
            })

    let (sems, masks) = case maySem of
            Nothing -> (V.empty, V.empty)
            Just sem -> (V.singleton sem, V.singleton Vk.PIPELINE_STAGE_FRAGMENT_SHADER_BIT)

    Vk.queueSubmit environmentGraphicsQueue
        (V.singleton $ Vk.SomeStruct Vk.SubmitInfo {
            Vk.next = (),
            Vk.waitSemaphores = sems,
            Vk.waitDstStageMask = masks,
            Vk.commandBuffers = V.singleton $ Vk.commandBufferHandle exchangeCommandBuffer,
            Vk.signalSemaphores = V.empty
        })
        exchangeFence

    pure (void $ Vk.waitForFences environmentDevice (V.singleton exchangeFence) True maxBound)