{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}

module Graphics.Pizza.Preparation where

import Control.Monad.IO.Class

import Data.Bits
import Data.Word

import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Array

-- linear
import Linear

-- vector
import qualified Data.Vector as V
import Data.Vector (Vector)

-- vulkan
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import qualified Vulkan.CStruct.Extends as Vk

-- VulkanMemoryAllocator
import qualified VulkanMemoryAllocator as Vma

-- pizza
import Graphics.Pizza.Renderer

-- | Rendering Operation
data Preparation = Preparation {
    preparationCommandBuffer :: Vk.CommandBuffer,
    preparationDescriptorSet :: Vk.DescriptorSet,
    preparationBufferVertexAlloc :: Vma.Allocation,
    preparationBufferVertex :: Vk.Buffer,
    preparationBufferIndexAlloc :: Vma.Allocation,
    preparationBufferIndex :: Vk.Buffer,
    preparationBufferUniformAlloc :: Vma.Allocation,
    preparationBufferUniform :: Vk.Buffer
}

newPreparation :: (MonadIO m) => Renderer -> m Preparation
newPreparation Renderer {..} = do
    let Environment {..} = rendererEnvironment
    commandBuffers <- Vk.allocateCommandBuffers
        environmentDevice
        Vk.zero { -- Vk.CommandBufferAllocateInfo
            Vk.commandPool = rendererCommandPool,
            Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY,
            Vk.commandBufferCount = 1
        }
    let preparationCommandBuffer = V.head commandBuffers

    descriptorSets <- Vk.allocateDescriptorSets
        environmentDevice
        Vk.zero { -- Vk.DescriptorSetAllocateInfo
            Vk.next = (),
            Vk.descriptorPool = rendererDescriptorPool,
            Vk.setLayouts = V.singleton rendererDescriptorSetLayout
        }
    let preparationDescriptorSet = V.head descriptorSets

    (preparationBufferVertex, preparationBufferVertexAlloc, _) <- Vma.createBuffer
        environmentAllocator
        Vk.zero { -- Vk.BufferCreateInfo
            Vk.size = fromIntegral $ 4 * sizeOf (undefined :: V2 Float),
            Vk.usage = Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT,
            Vk.sharingMode = Vk.SHARING_MODE_EXCLUSIVE,
            Vk.queueFamilyIndices = V.singleton environmentGraphicsQFI
        }
        Vk.zero { -- Vma.AllocationCreateInfo
            Vma.flags = Vma.ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT,
            Vma.usage = Vma.MEMORY_USAGE_AUTO
        }

    (preparationBufferIndex, preparationBufferIndexAlloc, _) <- Vma.createBuffer
        environmentAllocator
        Vk.zero { -- Vk.BufferCreateInfo
            Vk.size = fromIntegral $ 2 * sizeOf (undefined :: V3 Word32),
            Vk.usage = Vk.BUFFER_USAGE_INDEX_BUFFER_BIT,
            Vk.sharingMode = Vk.SHARING_MODE_EXCLUSIVE,
            Vk.queueFamilyIndices = V.singleton environmentGraphicsQFI
        }
        Vk.zero { -- Vma.AllocationCreateInfo
            Vma.flags = Vma.ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT,
            Vma.usage = Vma.MEMORY_USAGE_AUTO
        }

    (preparationBufferUniform, preparationBufferUniformAlloc, _) <- Vma.createBuffer
        environmentAllocator
        Vk.zero { -- Vk.BufferCreateInfo
            Vk.size = fromIntegral $ sizeOf (undefined :: V2 Float),
            Vk.usage = Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT,
            Vk.sharingMode = Vk.SHARING_MODE_EXCLUSIVE,
            Vk.queueFamilyIndices = V.singleton environmentGraphicsQFI
        }
        Vk.zero { -- Vma.AllocationCreateInfo
            Vma.flags = Vma.ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT,
            Vma.usage = Vma.MEMORY_USAGE_AUTO
        }

    -- Fill content
    -- TODO: Move each part to appropriate parts.

    Vk.updateDescriptorSets
        environmentDevice
        -- writes
        (V.singleton $ Vk.SomeStruct Vk.zero {
            Vk.next = (),
            Vk.dstSet = preparationDescriptorSet,
            Vk.dstBinding = 0,
            Vk.dstArrayElement = 0,
            Vk.descriptorCount = 1,
            Vk.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER,
            Vk.bufferInfo = V.singleton $ Vk.zero {
                Vk.buffer = preparationBufferUniform,
                Vk.offset = 0,
                Vk.range = Vk.WHOLE_SIZE
            }
        } )
        -- copies
        V.empty

    vertexPtr <- Vma.mapMemory environmentAllocator preparationBufferVertexAlloc
    liftIO $ pokeArray (castPtr vertexPtr) [V2 0 0, V2 200 0, V2 200 200, V2 0 200 :: V2 Float]
    Vma.unmapMemory environmentAllocator preparationBufferVertexAlloc

    indexPtr <- Vma.mapMemory environmentAllocator preparationBufferIndexAlloc
    liftIO $ pokeArray (castPtr indexPtr) [V3 0 1 2, V3 0 2 3 :: V3 Word32]
    Vma.unmapMemory environmentAllocator preparationBufferIndexAlloc

    -- We'll initialize uniforms at later.

    pure Preparation {..}

freePreparation :: (MonadIO m) => Renderer -> Preparation -> m ()
freePreparation Renderer {..} Preparation {..} = do
    let Environment {..} = rendererEnvironment
    Vma.destroyBuffer environmentAllocator preparationBufferUniform preparationBufferUniformAlloc
    Vma.destroyBuffer environmentAllocator preparationBufferIndex preparationBufferIndexAlloc
    Vma.destroyBuffer environmentAllocator preparationBufferVertex preparationBufferVertexAlloc
    Vk.freeDescriptorSets environmentDevice rendererDescriptorPool $ V.singleton preparationDescriptorSet
    Vk.freeCommandBuffers environmentDevice rendererCommandPool $ V.singleton preparationCommandBuffer

recordPreparation :: (MonadIO m) => Vk.CommandBuffer -> Renderer -> Preparation -> m ()
recordPreparation cmdbuf Renderer {..} Preparation {..} = do
    Vk.cmdBindPipeline cmdbuf Vk.PIPELINE_BIND_POINT_GRAPHICS rendererPipeline
    Vk.cmdBindIndexBuffer cmdbuf preparationBufferIndex 0 Vk.INDEX_TYPE_UINT32

    Vk.cmdBindVertexBuffers cmdbuf 0
        (V.singleton preparationBufferVertex)
        (V.singleton 0)

    Vk.cmdBindDescriptorSets cmdbuf
        Vk.PIPELINE_BIND_POINT_GRAPHICS
        rendererPipelineLayout
        0
        (V.singleton preparationDescriptorSet)
        (V.empty)

    Vk.cmdDrawIndexed preparationCommandBuffer 6 1 0 0 0

