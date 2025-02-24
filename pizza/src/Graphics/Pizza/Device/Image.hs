
{-# LANGUAGE RecordWildCards #-}

module Graphics.Pizza.Device.Image where

import Control.Monad.IO.Class
import Data.Bits
import Data.Traversable
import Foreign.Ptr

import qualified Data.Vector as V
import Data.Vector (Vector)

import qualified Vulkan as Vk
import qualified Vulkan.CStruct.Extends as Vk

import qualified VulkanMemoryAllocator as Vma

import Graphics.Pizza.Device.Environment
import Graphics.Pizza.Device.Format
import Graphics.Pizza.Device.RenderCore

data Image px = Image {
    imageSize :: Vk.Extent2D,
    imageObject :: Vk.Image,
    imageAlloc :: Vma.Allocation,
    imageView :: Vk.ImageView,
    imageSampler :: Vk.Sampler
}

newImage :: (MonadIO m, Format px) => Environment -> Int -> Int -> m (Image px)
newImage env width height = newImageV env width height undefined

newImageV :: (MonadIO m, Format px) => Environment -> Int -> Int -> px -> m (Image px)
newImageV Environment {..} width height pixel = do
    let widthw = fromIntegral width
        heightw = fromIntegral height
        imageSize = Vk.Extent2D widthw heightw
        imageFormat = formatOf pixel

    (imageObject, imageAlloc, _) <- Vma.createImage
        environmentAllocator
        Vk.ImageCreateInfo {
            Vk.next = (),
            Vk.flags = zeroBits,
            Vk.imageType = Vk.IMAGE_TYPE_2D,
            Vk.format = imageFormat,
            Vk.extent = Vk.Extent3D widthw heightw 1,
            Vk.mipLevels = 1,
            Vk.arrayLayers = 1,
            Vk.samples = Vk.SAMPLE_COUNT_1_BIT,
            Vk.tiling =Vk.IMAGE_TILING_OPTIMAL,
            Vk.usage = Vk.IMAGE_USAGE_SAMPLED_BIT .|. Vk.IMAGE_USAGE_TRANSFER_DST_BIT,
            Vk.sharingMode = Vk.SHARING_MODE_EXCLUSIVE,
            Vk.queueFamilyIndices = V.singleton environmentGraphicsQFI,
            Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
        }
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
    
    imageView <- Vk.createImageView
        environmentDevice
        Vk.ImageViewCreateInfo {
            Vk.next = (),
            Vk.flags = zeroBits,
            Vk.image = imageObject,
            Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D,
            Vk.format = imageFormat,
            Vk.components = Vk.ComponentMapping {
                Vk.r = Vk.COMPONENT_SWIZZLE_IDENTITY,
                Vk.g = Vk.COMPONENT_SWIZZLE_IDENTITY,
                Vk.b = Vk.COMPONENT_SWIZZLE_IDENTITY,
                Vk.a = Vk.COMPONENT_SWIZZLE_IDENTITY
            },
            Vk.subresourceRange = Vk.ImageSubresourceRange {
                Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT,
                Vk.baseMipLevel = 0,
                Vk.levelCount = 1,
                Vk.baseArrayLayer = 0,
                Vk.layerCount = 1
            }
        }
        Nothing
    
    imageSampler <- Vk.createSampler
        environmentDevice
        Vk.SamplerCreateInfo {
            Vk.next = (),
            Vk.flags = zeroBits,
            Vk.magFilter = Vk.FILTER_LINEAR,
            Vk.minFilter = Vk.FILTER_LINEAR,
            Vk.mipmapMode = Vk.SAMPLER_MIPMAP_MODE_LINEAR,
            Vk.addressModeU = Vk.SAMPLER_ADDRESS_MODE_REPEAT,
            Vk.addressModeV = Vk.SAMPLER_ADDRESS_MODE_REPEAT,
            Vk.addressModeW = Vk.SAMPLER_ADDRESS_MODE_REPEAT,
            Vk.mipLodBias = 0,
            Vk.anisotropyEnable = False, -- True
            Vk.maxAnisotropy = 1,
            Vk.compareEnable = False,
            Vk.compareOp = Vk.COMPARE_OP_ALWAYS,
            Vk.minLod = 0,
            Vk.maxLod = 0,
            Vk.borderColor = Vk.BORDER_COLOR_INT_OPAQUE_BLACK,
            Vk.unnormalizedCoordinates = False
        }
        Nothing

    pure Image {..}

freeImage :: (MonadIO m) => Environment -> Image px -> m ()
freeImage Environment {..} Image {..} = do
    Vk.destroySampler environmentDevice imageSampler Nothing
    Vk.destroyImageView environmentDevice imageView Nothing
    Vma.destroyImage environmentAllocator imageObject imageAlloc



newtype ImageSet px = ImageSet {
    imageSetDescriptorSets :: Vector Vk.DescriptorSet
}

newImageSet :: (MonadIO m) => RenderCore -> Vector (Image px) -> m (ImageSet px)
newImageSet RenderCore {..} images = do
    let Environment {..} = renderCoreEnvironment
    descriptorSets <- for images $ \img -> do
        descriptorSet <- Vk.allocateDescriptorSets
            environmentDevice
            Vk.DescriptorSetAllocateInfo {
                Vk.next = (),
                Vk.descriptorPool = renderCoreDescriptorPool,
                Vk.setLayouts = V.fromList [ renderCorePatternImageDSLayout ]
            }
        pure (img, V.head descriptorSet)
    
    let descriptorSetWrite = flip fmap descriptorSets $ \(img, ds) ->
            Vk.SomeStruct Vk.WriteDescriptorSet {
                Vk.next = (),
                Vk.dstSet = ds,
                Vk.dstBinding = 0,
                Vk.dstArrayElement = 0,
                Vk.descriptorCount = 1,
                Vk.descriptorType = Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                Vk.bufferInfo = V.empty,
                Vk.imageInfo = V.singleton Vk.DescriptorImageInfo {
                    Vk.sampler = imageSampler img,
                    Vk.imageView = imageView img,
                    Vk.imageLayout = Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
                },
                Vk.texelBufferView = V.empty
            }
    
    Vk.updateDescriptorSets
        environmentDevice
        descriptorSetWrite
        V.empty

    pure $ ImageSet $ fmap snd descriptorSets

freeImageSet :: (MonadIO m) => RenderCore -> ImageSet b -> m ()
freeImageSet RenderCore {..} ImageSet {..} =
    let Environment {..} = renderCoreEnvironment in
    Vk.freeDescriptorSets environmentDevice renderCoreDescriptorPool imageSetDescriptorSets

noImageSet :: ImageSet a
noImageSet = ImageSet V.empty