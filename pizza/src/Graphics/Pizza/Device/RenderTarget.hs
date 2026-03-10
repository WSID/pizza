{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}

module Graphics.Pizza.Device.RenderTarget where

import Control.Monad.IO.Class

import Data.Bits
import Data.Proxy
import Data.Foldable
import Data.Maybe
import Data.Traversable

import Foreign.Ptr

-- vector
import qualified Data.Vector as V
import Data.Vector (Vector, (!))

-- vulkan
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk

-- VulkanMemoryAllocator
import qualified VulkanMemoryAllocator as Vma

-- pizza
import Graphics.Pizza.Device.Environment
import Graphics.Pizza.Device.Format
import Graphics.Pizza.Device.Renderer
import Graphics.Pizza.Device.RenderCore
import Control.Monad

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

data SurfaceRenderTarget px = SurfaceRenderTarget {
    renderTargetSize :: Vk.Extent2D,
    renderTargetSwapchain :: Vk.SwapchainKHR,
    renderTargetAttachments :: RenderTargetAttachments,
    renderTargetBases :: Vector (BaseRenderTarget px, Vk.Semaphore)
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



newBaseRenderTarget :: (MonadIO m, Format px) => RenderCore -> Renderer px -> RenderTargetAttachments -> Vk.Image -> Int -> Int -> m (BaseRenderTarget px)
newBaseRenderTarget RenderCore {..} renderer attachments image width height = do
    let Environment {..} = renderCoreEnvironment
    renderTargetImageView <- Vk.createImageView
        environmentDevice
        Vk.ImageViewCreateInfo {
            Vk.next = (),
            Vk.flags = zeroBits,
            Vk.image = image,
            Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D,
            Vk.format = formatOf $ asProxyTypeOf undefined renderer,
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

    renderTargetFramebuffer <- renderTargetAttachmentsFramebuffer RenderCore {..} renderer attachments renderTargetImageView width height

    pure BaseRenderTarget {..}


freeBaseRenderTarget :: (MonadIO m) => RenderCore -> BaseRenderTarget px -> m ()
freeBaseRenderTarget RenderCore {..} BaseRenderTarget {..} = do
    let Environment {..} = renderCoreEnvironment
    Vk.destroyFramebuffer environmentDevice renderTargetFramebuffer Nothing
    Vk.destroyImageView environmentDevice renderTargetImageView Nothing


newRenderTarget :: (MonadIO m, Format px) => RenderCore -> Renderer px -> Int -> Int -> m (RenderTarget px)
newRenderTarget RenderCore {..} renderer width height = do
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
            Vk.format = formatOf $ asProxyTypeOf undefined renderer,
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

    renderTargetBase <- newBaseRenderTarget RenderCore {..} renderer renderTargetAttachments renderTargetImage width height

    pure RenderTarget {..}

freeRenderTarget :: (MonadIO m) => RenderCore -> RenderTarget px -> m ()
freeRenderTarget RenderCore {..} RenderTarget {..} = do
    let Environment {..} = renderCoreEnvironment
    freeBaseRenderTarget RenderCore {..} renderTargetBase
    freeRenderTargetAttachments RenderCore {..} renderTargetAttachments
    Vma.destroyImage environmentAllocator renderTargetImage renderTargetImageAlloc



newSurfaceRenderTarget :: (MonadIO m, Format px) => RenderCore -> Renderer px -> Vk.SurfaceKHR -> Int -> Int -> m (Maybe (SurfaceRenderTarget px))
newSurfaceRenderTarget RenderCore {..} renderer surface width height = do
    let Environment {..} = renderCoreEnvironment
    let renderTargetSize = Vk.Extent2D {
        Vk.width = fromIntegral width,
        Vk.height = fromIntegral height
    }
    let imageFormat = formatOf $ asProxyTypeOf undefined renderer

    (_, surfaceFormats) <- Vk.getPhysicalDeviceSurfaceFormatsKHR environmentPhysDevice surface
    let maySurfaceFormat = V.find (\Vk.SurfaceFormatKHR {..} -> format == imageFormat) surfaceFormats
    for maySurfaceFormat $ \surfaceFormat -> do
        let Vk.SurfaceFormatKHR {
            Vk.colorSpace = colorSpace
        } = surfaceFormat

        -- Capabilities
        capability <- Vk.getPhysicalDeviceSurfaceCapabilitiesKHR environmentPhysDevice surface
        
        let Vk.SurfaceCapabilitiesKHR {
            Vk.minImageCount = minImageCount
        } = capability

        let swapchainCreateInfo = Vk.SwapchainCreateInfoKHR {
            Vk.next = (),
            Vk.flags = zeroBits,
            Vk.surface = surface,
            Vk.minImageCount = minImageCount + 1,
            Vk.imageFormat = imageFormat,
            Vk.imageColorSpace = colorSpace,
            Vk.imageExtent = renderTargetSize,
            Vk.imageArrayLayers = 1,
            Vk.imageUsage = Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
            Vk.imageSharingMode = Vk.SHARING_MODE_EXCLUSIVE,
            Vk.queueFamilyIndices = V.empty,
            Vk.preTransform = Vk.SURFACE_TRANSFORM_IDENTITY_BIT_KHR,
            Vk.compositeAlpha = Vk.COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
            Vk.presentMode = Vk.PRESENT_MODE_FIFO_KHR,
            Vk.clipped = True,
            Vk.oldSwapchain = Vk.NULL_HANDLE
        }

        renderTargetSwapchain <- Vk.createSwapchainKHR environmentDevice swapchainCreateInfo Nothing
        renderTargetAttachments <- newRenderTargetAttachments RenderCore {..} width height 

        (_, images) <- Vk.getSwapchainImagesKHR environmentDevice renderTargetSwapchain

        renderTargetBases <- for images $ \image -> do
            renderTarget <- newBaseRenderTarget
                RenderCore {..}
                renderer
                renderTargetAttachments
                image
                width
                height
            semaphore <- Vk.createSemaphore
                environmentDevice
                Vk.SemaphoreCreateInfo {
                    Vk.next = (),
                    Vk.flags = zeroBits
                }
                Nothing
            pure (renderTarget, semaphore)

        pure SurfaceRenderTarget {..}

freeSurfaceRenderTarget :: (MonadIO m) => RenderCore -> SurfaceRenderTarget px -> m ()
freeSurfaceRenderTarget RenderCore {..} SurfaceRenderTarget {..} = do
    let Environment {..} = renderCoreEnvironment
    for_ renderTargetBases $ \(base, sem ) -> do
        freeBaseRenderTarget RenderCore {..} base
        Vk.destroySemaphore environmentDevice sem Nothing
    freeRenderTargetAttachments RenderCore {..} renderTargetAttachments
    Vk.destroySwapchainKHR environmentDevice renderTargetSwapchain Nothing

surfaceRenderTargetAcquireNext :: (MonadIO m) => RenderCore -> SurfaceRenderTarget px -> Maybe Vk.Semaphore -> m (Int, BaseRenderTarget px, Vk.Semaphore)
surfaceRenderTargetAcquireNext RenderCore {..} SurfaceRenderTarget {..} imageSem = do
    let Environment {..} = renderCoreEnvironment
    
    (_, indexw) <- Vk.acquireNextImageKHR
        environmentDevice
        renderTargetSwapchain
        maxBound -- timeout in nanosecs
        (fromMaybe Vk.NULL_HANDLE imageSem)
        Vk.NULL_HANDLE -- fence

    let index = fromIntegral indexw
        (base, sem) = renderTargetBases ! index
    
    pure (index, base, sem)


surfaceRenderTargetPresent :: (MonadIO m) => RenderCore -> SurfaceRenderTarget px -> Int -> m ()
surfaceRenderTargetPresent RenderCore {..} SurfaceRenderTarget {..} index = do
    let Environment {..} = renderCoreEnvironment
        waitSem = snd $ renderTargetBases ! index

    void $ Vk.queuePresentKHR
        environmentGraphicsQueue
        Vk.zero {
            Vk.waitSemaphores = V.singleton waitSem,
            Vk.swapchains = V.singleton renderTargetSwapchain,
            Vk.imageIndices = V.singleton (fromIntegral index)
        }