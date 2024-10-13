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
        "Patterns" ~: TestList [
            "Red" ~: do
                pixel <- makeRenderedImage (PatternSolid (V4 1 0 0 1))
                assert $ all (== V4 255 0 0 255) pixel
            ,
            "Green" ~: do
                pixel <- makeRenderedImage (PatternSolid (V4 0 1 0 1))
                assert $ all (== V4 0 255 0 255) pixel
            ,
            "Blue" ~: do
                pixel <- makeRenderedImage (PatternSolid (V4 0 0 1 1))
                assert $ all (== V4 0 0 255 255) pixel
        ]
    ]


data DevSelection = DevSelection {
    selectedDevice :: Vk.PhysicalDevice,
    selectedQFI :: Word32
}


makeRenderedImage :: Pattern -> IO [V4 Word8]
makeRenderedImage pattern = evalContT do
    -- Make use of ContT, with bracket.
    -- This will make such items to be freed at out of 'scope'.

    -- Environment
    environment <- ContT $ bracket newBasicEnvironment freeEnvironment
    let Environment {..} = environment

    let format = Vk.FORMAT_R8G8B8A8_UINT

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

    preparation <- ContT $ bracket
        (newPreparation renderer pattern)
        (freePreparation renderer)

    liftIO $ putStrLn "Pizza Preparation"

    renderState <- ContT $ bracket
        (newRenderState renderer)
        (freeRenderState renderer)

    liftIO $ putStrLn "Pizza Render State"


    -- Command Pool
    commandPool <- ContT $ Vk.withCommandPool environmentDevice
        -- Vk.CommandPoolCreateInfo
        Vk.zero { Vk.queueFamilyIndex = environmentGraphicsQFI }
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

    liftIO $ Vk.useCommandBuffer commandBuffer Vk.zero {
        Vk.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
    } $ do
        Vk.cmdCopyImageToBuffer commandBuffer
            (renderTargetImage renderTarget) Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
            staging
            (V.singleton $ Vk.zero {
                Vk.imageSubresource = Vk.zero {
                    Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT,
                    Vk.layerCount = 1
                },
                Vk.imageExtent = Vk.Extent3D 200 200 1
            })



    renderSem <- ContT $ Vk.withSemaphore environmentDevice Vk.zero Nothing bracket

    transferFence <- ContT $ Vk.withFence environmentDevice Vk.zero Nothing bracket

    renderRenderStateTarget renderer renderState preparation renderTarget Nothing

    Vk.queueSubmit environmentGraphicsQueue
        (V.singleton $ Vk.SomeStruct Vk.zero {
            Vk.waitSemaphores = V.singleton (renderStateSemaphore renderState),
            Vk.waitDstStageMask = V.singleton Vk.PIPELINE_STAGE_TRANSFER_BIT,
            Vk.commandBuffers = V.singleton $ Vk.commandBufferHandle commandBuffer
        })
        transferFence

    Vk.waitForFences environmentDevice (V.singleton transferFence) True maxBound

    ptr <- ContT $ Vma.withMappedMemory environmentAllocator stagingAlloc bracket

    let pixelPtr = castPtr ptr :: Ptr (V4 Word8)

    liftIO $ peekArray (200 * 200) pixelPtr

