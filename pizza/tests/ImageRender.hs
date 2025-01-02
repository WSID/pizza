{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}

module ImageRender where

-- base
import Data.Bits
import Data.Word

import Control.Monad.IO.Class
import Control.Exception (bracket)

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array

-- mtl / transformers
import Control.Monad.Trans.Cont

-- linear
import Linear

-- vector
import qualified Data.Vector as V

-- vulkan
import qualified Vulkan as Vk
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Zero as Vk

-- VulkanMemoryAllocator
import qualified VulkanMemoryAllocator as Vma

-- pizza
import Graphics.Pizza

makeRenderedImage :: Graphics -> IO [V4 Word8]
makeRenderedImage graphics = evalContT do
    -- Make use of ContT, with bracket.
    -- This will make such items to be freed at out of 'scope'.

    -- Environment
    environment <- ContT $ bracket newBasicEnvironment freeEnvironment
    let Environment {..} = environment

    let format = Vk.FORMAT_R8G8B8A8_UNORM

    -- Staging Buffer
    (staging, stagingAlloc, _) <- ContT $ Vma.withBuffer environmentAllocator
        -- Vk.BufferCreateInfo []
        Vk.zero {
            Vk.size = fromIntegral (200 * 200 * sizeOf (undefined :: V4 Word8)),
            Vk.usage = Vk.BUFFER_USAGE_TRANSFER_DST_BIT,
            Vk.sharingMode = Vk.SHARING_MODE_EXCLUSIVE,
            Vk.queueFamilyIndices = V.singleton environmentGraphicsQFI
        }
        -- Vma.AllocationCreateInfo
        Vk.zero {
            Vma.flags = Vma.ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT,
            Vma.usage = Vma.MEMORY_USAGE_AUTO
        }
        bracket

    -- Pizzas
    renderer <- ContT $ bracket
        (newRenderer environment format Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL)
        freeRenderer

    renderTarget <- ContT $ bracket
        (newRenderTarget renderer 200 200 format)
        (freeRenderTarget renderer)

    liftIO $ putStrLn "Pizza Preparation"

    renderState <- ContT $ bracket
        (newRenderState renderer)
        (freeRenderState renderer)

    liftIO $ putStrLn "Pizza Render State"


    -- Command Pool
    commandPool <- ContT $ Vk.withCommandPool environmentDevice
        Vk.CommandPoolCreateInfo {
            Vk.flags = zeroBits,
            Vk.queueFamilyIndex = environmentGraphicsQFI
        }
        Nothing bracket

    commandBuffers <- ContT $ Vk.withCommandBuffers environmentDevice
        -- Vk.CommandBufferAllocateInfo
        Vk.zero {
            Vk.commandPool = commandPool,
            Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY,
            Vk.commandBufferCount = 1
        }
        bracket

    let commandBuffer = V.head commandBuffers

    liftIO $ Vk.useCommandBuffer commandBuffer Vk.CommandBufferBeginInfo {
        Vk.next = (),
        Vk.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
        Vk.inheritanceInfo = Nothing
    } $ do
        Vk.cmdCopyImageToBuffer commandBuffer
            (renderTargetImage renderTarget) Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
            staging
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
                Vk.imageExtent = Vk.Extent3D 200 200 1
            })

    transferFence <- ContT $ Vk.withFence environmentDevice Vk.zero Nothing bracket

    _ <- renderRenderStateTarget renderer renderState graphics renderTarget Nothing

    Vk.queueSubmit environmentGraphicsQueue
        (V.singleton $ Vk.SomeStruct Vk.zero {
            Vk.waitSemaphores = V.singleton (renderStateSemaphore renderState),
            Vk.waitDstStageMask = V.singleton Vk.PIPELINE_STAGE_TRANSFER_BIT,
            Vk.commandBuffers = V.singleton $ Vk.commandBufferHandle commandBuffer
        })
        transferFence

    _ <- Vk.waitForFences environmentDevice (V.singleton transferFence) True maxBound

    ptr <- ContT $ Vma.withMappedMemory environmentAllocator stagingAlloc bracket

    let pixelPtr = castPtr ptr :: Ptr (V4 Word8)

    liftIO $ peekArray (200 * 200) pixelPtr

