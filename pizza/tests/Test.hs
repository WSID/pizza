{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}

module Main where

-- base
import Data.Bits
import Data.Word
import Data.Maybe
import Data.Function ((&))

import Control.Monad.IO.Class
import Control.Exception (bracket)

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array

-- HUnit
import Test.HUnit

-- mtl / transformers
import Control.Monad.Trans.Cont

-- bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

-- linear
import Linear

-- vector
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

-- vulkan
import qualified Vulkan as Vk
import qualified Vulkan.Dynamic as Vk
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Zero as Vk

-- vulkan-utils
import Vulkan.Utils.Misc ((.&&.))

-- VulkanMemoryAllocator
import qualified VulkanMemoryAllocator as Vma

-- pizza
import Graphics.Pizza


main :: IO ()
main = runTestTTAndExit $ TestList [
        "White Images" ~: do
            pixel <- makeRenderedImage
            assert $ all (== V4 255 255 255 255) pixel
    ]


data DevSelection = DevSelection {
    selectedDevice :: Vk.PhysicalDevice,
    selectedQFI :: Word32
}


makeRenderedImage :: IO [V4 Word8]
makeRenderedImage = evalContT do
    -- Make use of ContT, with bracket.
    -- This will make such items to be freed at out of 'scope'.

    -- Instance
    inst <- ContT $ Vk.withInstance
        -- Vk.InstanceCreateInfo
        Vk.zero {
            Vk.applicationInfo = Just Vk.zero { -- Vk.ApplicationInfo
                Vk.applicationName = Just $ BSC.pack "pizza-test"
            },
            Vk.enabledLayerNames = V.singleton $ BSC.pack "VK_LAYER_KHRONOS_validation"
        }
        Nothing bracket


    -- Physical Devices
    (_, pdevices) <- Vk.enumeratePhysicalDevices inst
    selections <- pdevices
        & V.mapMaybeM \pdev -> do
            qprops <- Vk.getPhysicalDeviceQueueFamilyProperties pdev

            let mindex = qprops
                    & V.findIndex \Vk.QueueFamilyProperties {..} ->
                        queueFlags .&&. Vk.QUEUE_GRAPHICS_BIT

            pure (DevSelection pdev . fromIntegral <$> mindex)

    let DevSelection {..} = V.head selections


    -- Device
    -- Don't use any extension. This just need to render on plain image.
    device <- ContT $ Vk.withDevice selectedDevice
        -- Vk.DeviceCreateInfo
        Vk.zero {
            -- Vk.DeviceQueueCreateInfo
            Vk.queueCreateInfos = V.singleton $ Vk.SomeStruct Vk.zero {
                Vk.queueFamilyIndex = selectedQFI,
                Vk.queuePriorities = V.singleton 1.0
            }
        }
        Nothing bracket

    queue <- Vk.getDeviceQueue device selectedQFI 0

    -- Allocator
    let Vk.Instance {
        Vk.instanceCmds = instFuncs
    } = inst

    let Vk.Device {
        Vk.deviceCmds = devFuncs
    } = device

    allocator <- ContT $ Vma.withAllocator
        -- Vma.AllocatorCreateInfo
        Vk.zero {
            Vma.physicalDevice = Vk.physicalDeviceHandle selectedDevice,
            Vma.device = Vk.deviceHandle device,
            Vma.instance' = Vk.instanceHandle inst,
            Vma.vulkanFunctions = Just Vk.zero {
                Vma.vkGetInstanceProcAddr = castFunPtr $ Vk.pVkGetInstanceProcAddr instFuncs,
                Vma.vkGetDeviceProcAddr = castFunPtr $ Vk.pVkGetDeviceProcAddr devFuncs
            }
        }
        bracket

    let format = Vk.FORMAT_R8G8B8A8_UINT

    -- Images
    (image, imageAlloc, _) <- ContT $ Vma.withImage allocator
        -- Vk.ImageCreateInfo []
        Vk.zero {
            Vk.imageType = Vk.IMAGE_TYPE_2D,
            Vk.format = format,
            Vk.extent = Vk.Extent3D 200 200 1,
            Vk.mipLevels = 1,
            Vk.arrayLayers = 1,
            Vk.samples = Vk.SAMPLE_COUNT_1_BIT,
            Vk.tiling = Vk.IMAGE_TILING_OPTIMAL,
            Vk.usage = Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. Vk.IMAGE_USAGE_TRANSFER_SRC_BIT,
            Vk.sharingMode = Vk.SHARING_MODE_EXCLUSIVE,
            Vk.queueFamilyIndices = V.singleton selectedQFI,
            Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
        }
        -- Vma.AllocationCreateInfo
        Vk.zero {
            Vma.usage = Vma.MEMORY_USAGE_AUTO
        }
        bracket

    -- Staging Buffer
    (staging, stagingAlloc, _) <- ContT $ Vma.withBuffer allocator
        -- Vk.BufferCreateInfo []
        Vk.zero {
            Vk.size = fromIntegral (200 * 200 * sizeOf (undefined :: V4 Word8)),
            Vk.usage = Vk.BUFFER_USAGE_TRANSFER_DST_BIT,
            Vk.sharingMode = Vk.SHARING_MODE_EXCLUSIVE,
            Vk.queueFamilyIndices = V.singleton selectedQFI
        }
        -- Vma.AllocationCreateInfo
        Vk.zero {
            Vma.flags = Vma.ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT,
            Vma.usage = Vma.MEMORY_USAGE_AUTO
        }
        bracket


    -- Command Pool
    commandPool <- ContT $ Vk.withCommandPool device
        -- Vk.CommandPoolCreateInfo
        Vk.zero { Vk.queueFamilyIndex = selectedQFI }
        Nothing bracket

    commandBuffers <- ContT $ Vk.withCommandBuffers device
        -- Vk.CommandBufferAllocateInfo
        Vk.zero {
            Vk.commandPool = commandPool,
            Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY,
            Vk.commandBufferCount = 1
        }
        bracket

    let commandBuffer = V.head commandBuffers

    liftIO $ Vk.useCommandBuffer commandBuffer Vk.zero {
        Vk.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
    } $ do
        Vk.cmdCopyImageToBuffer commandBuffer
            image Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
            staging
            (V.singleton $ Vk.zero {
                Vk.imageSubresource = Vk.zero {
                    Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT,
                    Vk.layerCount = 1
                },
                Vk.imageExtent = Vk.Extent3D 200 200 1
            })

    -- Pizzas
    context <- ContT $ bracket
        (newContext device selectedQFI format Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL)
        freeContext

    renderTarget <- ContT $ bracket
        (newRenderTarget context image format 200 200)
        (freeRenderTarget context)

    renderState <- ContT $ bracket
        (newRender allocator context)
        (freeRender context)

    renderSem <- ContT $ Vk.withSemaphore device Vk.zero Nothing bracket

    transferFence <- ContT $ Vk.withFence device Vk.zero Nothing bracket

    render queue Nothing (V.singleton renderSem) Vk.NULL_HANDLE context renderState renderTarget

    Vk.queueSubmit queue
        (V.singleton $ Vk.SomeStruct Vk.zero {
            Vk.waitSemaphores = V.singleton renderSem,
            Vk.waitDstStageMask = V.singleton Vk.PIPELINE_STAGE_TRANSFER_BIT,
            Vk.commandBuffers = V.singleton $ Vk.commandBufferHandle commandBuffer
        })
        transferFence

    Vk.waitForFences device (V.singleton transferFence) True maxBound

    ptr <- ContT $ Vma.withMappedMemory allocator stagingAlloc bracket

    let pixelPtr = castPtr ptr :: Ptr (V4 Word8)

    liftIO $ peekArray (200 * 200) pixelPtr
