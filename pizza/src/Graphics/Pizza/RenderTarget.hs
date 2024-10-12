{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}

module Graphics.Pizza.RenderTarget where

import Control.Monad.IO.Class

import Data.Bits
import Data.Foldable
import Data.Traversable

-- linear
import Linear

-- vector
import qualified Data.Vector as V
import Data.Vector (Vector)

-- vulkan
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Version as Vk

-- VulkanMemoryAllocator
import qualified VulkanMemoryAllocator as Vma

-- pizza
import Graphics.Pizza.Renderer
import Graphics.Pizza.Preparation

data BaseRenderTarget = BaseRenderTarget {
    renderTargetImageView :: Vk.ImageView,
    renderTargetFramebuffer :: Vk.Framebuffer
}

data RenderTarget = RenderTarget {
    renderTargetSize :: Vk.Extent2D,
    renderTargetImage :: Vk.Image,
    renderTargetImageAlloc :: Vma.Allocation,
    renderTargetBase :: BaseRenderTarget
}

data SwapchainRenderTarget = SwapchainRenderTarget {
    renderTargetSize :: Vk.Extent2D,
    renderTargetSwapchain :: Vk.SwapchainKHR,
    renderTargetBase :: Vector BaseRenderTarget
}

newBaseRenderTarget :: (MonadIO m) => Environment -> Renderer -> Vk.Image -> Int -> Int -> Vk.Format -> m BaseRenderTarget
newBaseRenderTarget Environment {..} Renderer {..} image width height imageFormat = do
    renderTargetImageView <- Vk.createImageView
        environmentDevice
        Vk.ImageViewCreateInfo {
            Vk.next = (),
            Vk.flags = zeroBits,
            Vk.image = image,
            Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D,
            Vk.format = imageFormat,
            Vk.components = Vk.ComponentMapping
                Vk.COMPONENT_SWIZZLE_IDENTITY
                Vk.COMPONENT_SWIZZLE_IDENTITY
                Vk.COMPONENT_SWIZZLE_IDENTITY
                Vk.COMPONENT_SWIZZLE_IDENTITY,
            Vk.subresourceRange = Vk.ImageSubresourceRange {
                Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT,
                Vk.baseMipLevel = 0,
                Vk.levelCount = 1,
                Vk.baseArrayLayer = 0,
                Vk.layerCount = 1
            }
        }
        Nothing

    renderTargetFramebuffer <- Vk.createFramebuffer
        environmentDevice
        Vk.zero {
            Vk.renderPass = rendererRenderPass,
            Vk.attachments = V.singleton renderTargetImageView,
            Vk.width = fromIntegral width,
            Vk.height = fromIntegral height,
            Vk.layers = 1
        }
        Nothing

    pure BaseRenderTarget {..}


freeBaseRenderTarget :: (MonadIO m) => Environment -> BaseRenderTarget -> m ()
freeBaseRenderTarget Environment {..} BaseRenderTarget {..} = do
    Vk.destroyFramebuffer environmentDevice renderTargetFramebuffer Nothing
    Vk.destroyImageView environmentDevice renderTargetImageView Nothing

recordBaseRenderTarget :: (MonadIO m) => Vk.CommandBuffer -> Renderer -> Int -> Int -> BaseRenderTarget -> m r -> m r
recordBaseRenderTarget cmdbuf Renderer {..} width height BaseRenderTarget {..} inside = do
    let renderArea = Vk.Rect2D {
        Vk.offset = Vk.Offset2D 0 0,
        Vk.extent = Vk.Extent2D (fromIntegral width) (fromIntegral height)
    }

    Vk.resetCommandBuffer cmdbuf zeroBits

    Vk.useCommandBuffer cmdbuf Vk.zero {
        Vk.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
    } $ do
        Vk.cmdSetViewport cmdbuf 0 $ V.singleton Vk.Viewport {
            Vk.x = 0,
            Vk.y = 0,
            Vk.width = fromIntegral width,
            Vk.height = fromIntegral height,
            Vk.minDepth = 0,
            Vk.maxDepth = 1
        }

        Vk.cmdSetScissor cmdbuf 0 $ V.singleton renderArea

        Vk.cmdUseRenderPass
            cmdbuf
            Vk.zero {
                Vk.renderPass = rendererRenderPass,
                Vk.framebuffer = renderTargetFramebuffer,
                Vk.renderArea = renderArea,
                Vk.clearValues = V.singleton (Vk.Color $ Vk.Float32 0 0 0 1)
            }
            Vk.SUBPASS_CONTENTS_INLINE
            inside

newRenderTarget :: (MonadIO m) => Environment -> Renderer -> Int -> Int -> Vk.Format -> m RenderTarget
newRenderTarget Environment {..} Renderer {..} width height imageFormat = do
    let widthw = fromIntegral width
        heightw = fromIntegral height
    let renderTargetSize = Vk.Extent2D widthw heightw

    (renderTargetImage, renderTargetImageAlloc, _) <- Vma.createImage
        environmentAllocator
        -- Vk.ImageCreateInfo []
        Vk.zero {
            Vk.imageType = Vk.IMAGE_TYPE_2D,
            Vk.format = imageFormat,
            Vk.extent = Vk.Extent3D widthw heightw 1,
            Vk.mipLevels = 1,
            Vk.arrayLayers = 1,
            Vk.samples = Vk.SAMPLE_COUNT_1_BIT,
            Vk.tiling = Vk.IMAGE_TILING_OPTIMAL,
            Vk.usage = Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. Vk.IMAGE_USAGE_TRANSFER_SRC_BIT,
            Vk.sharingMode = Vk.SHARING_MODE_EXCLUSIVE,
            Vk.queueFamilyIndices = V.singleton environmentGraphicsQFI,
            Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
        }
        -- Vma.AllocationCreateInfo
        Vk.zero {
            Vma.usage = Vma.MEMORY_USAGE_AUTO
        }

    renderTargetBase <- newBaseRenderTarget Environment {..} Renderer {..} renderTargetImage width height imageFormat

    pure RenderTarget {..}

freeRenderTarget :: (MonadIO m) => Environment -> RenderTarget -> m ()
freeRenderTarget Environment {..} RenderTarget {..} = do
    freeBaseRenderTarget Environment {..} renderTargetBase
    Vma.destroyImage environmentAllocator renderTargetImage renderTargetImageAlloc

recordRenderTarget :: (MonadIO m) => Vk.CommandBuffer -> Renderer -> RenderTarget -> m r -> m r
recordRenderTarget cmdbuf renderer RenderTarget {..} =
    recordBaseRenderTarget cmdbuf renderer (fromIntegral width) (fromIntegral height) renderTargetBase
  where
    Vk.Extent2D {
        Vk.width = width,
        Vk.height = height
    } = renderTargetSize


newSwapchainRenderTarget :: (MonadIO m) => Environment -> Renderer -> Vk.SwapchainKHR -> Int -> Int -> Vk.Format -> m SwapchainRenderTarget
newSwapchainRenderTarget Environment {..} Renderer {..} renderTargetSwapchain width height imageFormat = do
    let widthw = fromIntegral width
        heightw = fromIntegral height
    let renderTargetSize = Vk.Extent2D widthw heightw

    (_, images) <- Vk.getSwapchainImagesKHR environmentDevice renderTargetSwapchain

    renderTargetBase <- traverse
        (\image -> newBaseRenderTarget Environment {..} Renderer {..} image width height imageFormat)
        images

    pure SwapchainRenderTarget {..}

freeSwapchainRenderTarget :: (MonadIO m) => Environment -> SwapchainRenderTarget -> m ()
freeSwapchainRenderTarget Environment {..} SwapchainRenderTarget {..} = do
    traverse_ (freeBaseRenderTarget Environment {..}) renderTargetBase


