{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}

module ImageRender where

-- base
import Data.Bits
import Data.Word

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array

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
makeRenderedImage graphics = do

    -- Environment
    environment <- newBasicEnvironment
    let Environment {..} = environment

    let format = Vk.FORMAT_R8G8B8A8_UNORM

    -- Staging Buffer
    (staging, stagingAlloc, _) <- Vma.createBuffer environmentAllocator
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

    -- Pizzas
    renderer <- newRenderer environment format Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
    renderTarget <- newRenderTarget renderer 200 200 format
    renderState <- newRenderState renderer


    -- Command Pool
    commandPool <- Vk.createCommandPool environmentDevice
        Vk.CommandPoolCreateInfo {
            Vk.flags = zeroBits,
            Vk.queueFamilyIndex = environmentGraphicsQFI
        }
        Nothing

    commandBuffers <- Vk.allocateCommandBuffers environmentDevice
        -- Vk.CommandBufferAllocateInfo
        Vk.zero {
            Vk.commandPool = commandPool,
            Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY,
            Vk.commandBufferCount = 1
        }

    let commandBuffer = V.head commandBuffers

    Vk.useCommandBuffer commandBuffer Vk.CommandBufferBeginInfo {
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

    transferFence <- Vk.createFence environmentDevice Vk.zero Nothing

    _ <- renderRenderStateTarget renderer renderState graphics renderTarget Nothing

    Vk.queueSubmit environmentGraphicsQueue
        (V.singleton $ Vk.SomeStruct Vk.zero {
            Vk.waitSemaphores = V.singleton (renderStateSemaphore renderState),
            Vk.waitDstStageMask = V.singleton Vk.PIPELINE_STAGE_TRANSFER_BIT,
            Vk.commandBuffers = V.singleton $ Vk.commandBufferHandle commandBuffer
        })
        transferFence

    _ <- Vk.waitForFences environmentDevice (V.singleton transferFence) True maxBound

    ptr <- Vma.mapMemory environmentAllocator stagingAlloc

    let pixelPtr = castPtr ptr :: Ptr (V4 Word8)
    result <- peekArray (200 * 200) pixelPtr

    -- Unmap Mapped Memory and Free Resources
    Vma.unmapMemory environmentAllocator stagingAlloc

    Vk.destroyFence environmentDevice transferFence Nothing
    Vk.freeCommandBuffers environmentDevice commandPool commandBuffers
    Vk.destroyCommandPool environmentDevice commandPool Nothing

    freeRenderState renderer renderState
    freeRenderTarget renderer renderTarget
    freeRenderer renderer

    Vma.destroyBuffer environmentAllocator staging stagingAlloc

    freeEnvironment environment

    pure result
