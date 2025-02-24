{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}

module Graphics.Pizza.Device.RenderTarget where

import Control.Monad.IO.Class

import Data.Bits
import Data.Foldable

import Foreign.Ptr

-- vector
import qualified Data.Vector as V
import Data.Vector (Vector)

-- vulkan
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk

-- VulkanMemoryAllocator
import qualified VulkanMemoryAllocator as Vma

-- pizza
import Graphics.Pizza.Device.Environment
import Graphics.Pizza.Device.Renderer
import Graphics.Pizza.Device.RenderCore

data RenderTargetAttachments = RenderTargetAttachments {
    renderTargetStencil :: Vk.Image,
    renderTargetStencilAlloc :: Vma.Allocation,
    renderTargetStencilView :: Vk.ImageView
}

data BaseRenderTarget px = BaseRenderTarget {
    renderTargetImageView :: Vk.ImageView,
    renderTargetFramebuffer :: Vk.Framebuffer
}

data RenderTarget px = RenderTarget {
    renderTargetSize :: Vk.Extent2D,
    renderTargetImage :: Vk.Image,
    renderTargetImageAlloc :: Vma.Allocation,
    renderTargetAttachments :: RenderTargetAttachments,
    renderTargetBase :: BaseRenderTarget px
}

data SwapchainRenderTarget px = SwapchainRenderTarget {
    renderTargetSize :: Vk.Extent2D,
    renderTargetSwapchain :: Vk.SwapchainKHR,
    renderTargetAttachments :: RenderTargetAttachments,
    renderTargetBase :: Vector (BaseRenderTarget px)
}

newRenderTargetAttachments :: (MonadIO m) => RenderCore -> Int -> Int -> m RenderTargetAttachments
newRenderTargetAttachments RenderCore {..} width height = do
    let Environment {..} = renderCoreEnvironment
    (renderTargetStencil, renderTargetStencilAlloc, _) <- Vma.createImage
        environmentAllocator
        Vk.ImageCreateInfo {
            Vk.next = (),
            Vk.flags = zeroBits,
            Vk.imageType = Vk.IMAGE_TYPE_2D,
            Vk.format = Vk.FORMAT_S8_UINT,
            Vk.extent = Vk.Extent3D (fromIntegral width) (fromIntegral height) 1,
            Vk.mipLevels = 1,
            Vk.arrayLayers = 1,
            Vk.samples = Vk.SAMPLE_COUNT_1_BIT,
            Vk.tiling = Vk.IMAGE_TILING_OPTIMAL,
            Vk.usage = Vk.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT,
            Vk.sharingMode = Vk.SHARING_MODE_EXCLUSIVE,
            Vk.queueFamilyIndices = V.singleton environmentGraphicsQFI,
            Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
        }
        Vma.AllocationCreateInfo {
            Vma.flags = zeroBits,
            Vma.usage = Vma.MEMORY_USAGE_AUTO,
            Vma.requiredFlags = zeroBits,
            Vma.preferredFlags = Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
            Vma.memoryTypeBits = 0,
            Vma.pool = Vk.NULL_HANDLE,
            Vma.userData = nullPtr,
            Vma.priority = 0
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

    pure RenderTargetAttachments {..}

freeRenderTargetAttachments :: (MonadIO m) => RenderCore -> RenderTargetAttachments -> m ()
freeRenderTargetAttachments RenderCore {..} RenderTargetAttachments {..} = do
    let Environment {..} = renderCoreEnvironment
    Vk.destroyImageView environmentDevice renderTargetStencilView Nothing
    Vma.destroyImage environmentAllocator renderTargetStencil renderTargetStencilAlloc

renderTargetAttachmentsFramebuffer :: (MonadIO m) => RenderCore -> Renderer px -> RenderTargetAttachments -> Vk.ImageView -> Int -> Int ->  m Vk.Framebuffer
renderTargetAttachmentsFramebuffer RenderCore {..} Renderer {..} RenderTargetAttachments {..} imageView width height =
    Vk.createFramebuffer
        environmentDevice
        Vk.zero {
            Vk.renderPass = rendererRenderPass,
            Vk.attachments = V.fromList [imageView, renderTargetStencilView],
            Vk.width = fromIntegral width,
            Vk.height = fromIntegral height,
            Vk.layers = 1
        }
        Nothing
  where
    Environment {..} = renderCoreEnvironment



newBaseRenderTarget :: (MonadIO m) => RenderCore -> Renderer px -> RenderTargetAttachments -> Vk.Image -> Int -> Int -> m (BaseRenderTarget px)
newBaseRenderTarget RenderCore {..} Renderer {..} attachments image width height = do
    let Environment {..} = renderCoreEnvironment
    renderTargetImageView <- Vk.createImageView
        environmentDevice
        Vk.ImageViewCreateInfo {
            Vk.next = (),
            Vk.flags = zeroBits,
            Vk.image = image,
            Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D,
            Vk.format = rendererImageFormat,
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

    renderTargetFramebuffer <- renderTargetAttachmentsFramebuffer RenderCore {..} Renderer {..} attachments renderTargetImageView width height

    pure BaseRenderTarget {..}


freeBaseRenderTarget :: (MonadIO m) => RenderCore -> BaseRenderTarget px -> m ()
freeBaseRenderTarget RenderCore {..} BaseRenderTarget {..} = do
    let Environment {..} = renderCoreEnvironment
    Vk.destroyFramebuffer environmentDevice renderTargetFramebuffer Nothing
    Vk.destroyImageView environmentDevice renderTargetImageView Nothing

recordBaseRenderTarget :: (MonadIO m) => Vk.CommandBuffer -> Renderer px -> Int -> Int -> BaseRenderTarget px -> m r -> m r
recordBaseRenderTarget cmdbuf Renderer {..} width height BaseRenderTarget {..} inside = do
    let renderArea = Vk.Rect2D {
        Vk.offset = Vk.Offset2D 0 0,
        Vk.extent = Vk.Extent2D (fromIntegral width) (fromIntegral height)
    }

    Vk.resetCommandBuffer cmdbuf zeroBits

    Vk.useCommandBuffer cmdbuf Vk.CommandBufferBeginInfo {
        Vk.next = (),
        Vk.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
        Vk.inheritanceInfo = Nothing
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



newRenderTarget :: (MonadIO m) => RenderCore -> Renderer px -> Int -> Int -> m (RenderTarget px)
newRenderTarget RenderCore {..} Renderer {..} width height = do
    let Environment {..} = renderCoreEnvironment
    let widthw = fromIntegral width
        heightw = fromIntegral height
    let renderTargetSize = Vk.Extent2D widthw heightw

    (renderTargetImage, renderTargetImageAlloc, _) <- Vma.createImage
        environmentAllocator
        Vk.ImageCreateInfo {
            Vk.next = (),
            Vk.flags = zeroBits,
            Vk.imageType = Vk.IMAGE_TYPE_2D,
            Vk.format = rendererImageFormat,
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
        Vma.AllocationCreateInfo {
            Vma.flags = zeroBits,
            Vma.usage = Vma.MEMORY_USAGE_AUTO,
            Vma.requiredFlags = zeroBits,
            Vma.preferredFlags = zeroBits,
            Vma.memoryTypeBits = 0,
            Vma.pool = Vk.NULL_HANDLE,
            Vma.userData = nullPtr,
            Vma.priority = 0
        }
    
    renderTargetAttachments <- newRenderTargetAttachments RenderCore {..} width height

    renderTargetBase <- newBaseRenderTarget RenderCore {..} Renderer {..} renderTargetAttachments renderTargetImage width height

    pure RenderTarget {..}

freeRenderTarget :: (MonadIO m) => RenderCore -> RenderTarget px -> m ()
freeRenderTarget RenderCore {..} RenderTarget {..} = do
    let Environment {..} = renderCoreEnvironment
    freeBaseRenderTarget RenderCore {..} renderTargetBase
    freeRenderTargetAttachments RenderCore {..} renderTargetAttachments
    Vma.destroyImage environmentAllocator renderTargetImage renderTargetImageAlloc

recordRenderTarget :: (MonadIO m) => Vk.CommandBuffer -> Renderer px -> RenderTarget px -> m r -> m r
recordRenderTarget cmdbuf renderer RenderTarget {..} =
    recordBaseRenderTarget cmdbuf renderer (fromIntegral width) (fromIntegral height) renderTargetBase
  where
    Vk.Extent2D {
        Vk.width = width,
        Vk.height = height
    } = renderTargetSize




newSwapchainRenderTarget :: (MonadIO m) => RenderCore -> Renderer px -> Vk.SwapchainKHR -> Int -> Int -> m (SwapchainRenderTarget px)
newSwapchainRenderTarget RenderCore {..} Renderer {..} renderTargetSwapchain width height = do
    let Environment {..} = renderCoreEnvironment
    let widthw = fromIntegral width
        heightw = fromIntegral height
    let renderTargetSize = Vk.Extent2D widthw heightw

    renderTargetAttachments <- newRenderTargetAttachments RenderCore {..} width height

    (_, images) <- Vk.getSwapchainImagesKHR environmentDevice renderTargetSwapchain

    renderTargetBase <- traverse
        (\image -> newBaseRenderTarget RenderCore {..} Renderer {..} renderTargetAttachments image width height)
        images

    pure SwapchainRenderTarget {..}

freeSwapchainRenderTarget :: (MonadIO m) => RenderCore -> SwapchainRenderTarget px -> m ()
freeSwapchainRenderTarget RenderCore {..} SwapchainRenderTarget {..} = do
    traverse_ (freeBaseRenderTarget RenderCore {..}) renderTargetBase
    freeRenderTargetAttachments RenderCore {..} renderTargetAttachments


