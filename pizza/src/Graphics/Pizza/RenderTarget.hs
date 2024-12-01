{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}

module Graphics.Pizza.RenderTarget where

import Control.Monad.IO.Class

import Data.Bits
import Data.Foldable

-- vector
import qualified Data.Vector as V
import Data.Vector (Vector)

-- vulkan
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk

-- VulkanMemoryAllocator
import qualified VulkanMemoryAllocator as Vma

-- pizza
import Graphics.Pizza.Environment
import Graphics.Pizza.Renderer

data BaseRenderTarget = BaseRenderTarget {
    renderTargetImageView :: Vk.ImageView,
    renderTargetStencil :: Vk.Image,
    renderTargetStencilAlloc :: Vma.Allocation,
    renderTargetStencilView :: Vk.ImageView,
    renderTargetStencilBuffer :: Vk.Framebuffer,
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

newBaseRenderTarget :: (MonadIO m) => Renderer -> Vk.Image -> Int -> Int -> Vk.Format -> m BaseRenderTarget
newBaseRenderTarget Renderer {..} image width height imageFormat = do
    let Environment {..} = rendererEnvironment
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

    (renderTargetStencil, renderTargetStencilAlloc, _) <- Vma.createImage
        environmentAllocator
        (Vk.zero :: Vk.ImageCreateInfo '[]) {
            Vk.imageType = Vk.IMAGE_TYPE_2D,
            Vk.format = Vk.FORMAT_S8_UINT,
            Vk.extent = Vk.Extent3D (fromIntegral width) (fromIntegral height) 1,
            Vk.mipLevels = 1,
            Vk.arrayLayers = 1,
            Vk.samples = Vk.SAMPLE_COUNT_1_BIT,
            Vk.usage = Vk.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT,
            Vk.queueFamilyIndices = V.singleton environmentGraphicsQFI,
            Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
        }
        (Vk.zero :: Vma.AllocationCreateInfo) {
            Vma.preferredFlags = Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT
        }

    renderTargetStencilView <- Vk.createImageView
        environmentDevice
        Vk.ImageViewCreateInfo {
            Vk.next = (),
            Vk.flags = zeroBits,
            Vk.image = renderTargetStencil,
            Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D,
            Vk.format = Vk.FORMAT_S8_UINT,
            Vk.components = Vk.ComponentMapping
                Vk.COMPONENT_SWIZZLE_IDENTITY
                Vk.COMPONENT_SWIZZLE_IDENTITY
                Vk.COMPONENT_SWIZZLE_IDENTITY
                Vk.COMPONENT_SWIZZLE_IDENTITY,
            Vk.subresourceRange = Vk.ImageSubresourceRange {
                Vk.aspectMask = Vk.IMAGE_ASPECT_STENCIL_BIT,
                Vk.baseMipLevel = 0,
                Vk.levelCount = 1,
                Vk.baseArrayLayer = 0,
                Vk.layerCount = 1
            }
        }
        Nothing

    renderTargetStencilBuffer <- Vk.createFramebuffer
        environmentDevice
        Vk.zero {
            Vk.renderPass = rendererStencilRenderPass,
            Vk.attachments = V.singleton renderTargetStencilView,
            Vk.width = fromIntegral width,
            Vk.height = fromIntegral height,
            Vk.layers = 1
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


freeBaseRenderTarget :: (MonadIO m) => Renderer -> BaseRenderTarget -> m ()
freeBaseRenderTarget Renderer {..} BaseRenderTarget {..} = do
    let Environment {..} = rendererEnvironment
    Vk.destroyFramebuffer environmentDevice renderTargetFramebuffer Nothing
    Vk.destroyFramebuffer environmentDevice renderTargetStencilBuffer Nothing
    Vk.destroyImageView environmentDevice renderTargetStencilView Nothing
    Vma.destroyImage environmentAllocator renderTargetStencil renderTargetStencilAlloc
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

newRenderTarget :: (MonadIO m) => Renderer -> Int -> Int -> Vk.Format -> m RenderTarget
newRenderTarget Renderer {..} width height imageFormat = do
    let Environment {..} = rendererEnvironment
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

    renderTargetBase <- newBaseRenderTarget Renderer {..} renderTargetImage width height imageFormat

    pure RenderTarget {..}

freeRenderTarget :: (MonadIO m) => Renderer -> RenderTarget -> m ()
freeRenderTarget Renderer {..} RenderTarget {..} = do
    let Environment {..} = rendererEnvironment
    freeBaseRenderTarget Renderer {..} renderTargetBase
    Vma.destroyImage environmentAllocator renderTargetImage renderTargetImageAlloc

recordRenderTarget :: (MonadIO m) => Vk.CommandBuffer -> Renderer -> RenderTarget -> m r -> m r
recordRenderTarget cmdbuf renderer RenderTarget {..} =
    recordBaseRenderTarget cmdbuf renderer (fromIntegral width) (fromIntegral height) renderTargetBase
  where
    Vk.Extent2D {
        Vk.width = width,
        Vk.height = height
    } = renderTargetSize


newSwapchainRenderTarget :: (MonadIO m) => Renderer -> Vk.SwapchainKHR -> Int -> Int -> Vk.Format -> m SwapchainRenderTarget
newSwapchainRenderTarget Renderer {..} renderTargetSwapchain width height imageFormat = do
    let Environment {..} = rendererEnvironment
    let widthw = fromIntegral width
        heightw = fromIntegral height
    let renderTargetSize = Vk.Extent2D widthw heightw

    (_, images) <- Vk.getSwapchainImagesKHR environmentDevice renderTargetSwapchain

    renderTargetBase <- traverse
        (\image -> newBaseRenderTarget Renderer {..} image width height imageFormat)
        images

    pure SwapchainRenderTarget {..}

freeSwapchainRenderTarget :: (MonadIO m) => Renderer -> SwapchainRenderTarget -> m ()
freeSwapchainRenderTarget Renderer {..} SwapchainRenderTarget {..} =
    traverse_ (freeBaseRenderTarget Renderer {..}) renderTargetBase


