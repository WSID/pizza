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
import Graphics.Pizza.Internal.TypedBuffer

-- | Rendering Operation
data Preparation = Preparation {
    preparationCommandBuffer :: Vk.CommandBuffer,
    preparationDescriptorSet :: Vk.DescriptorSet,
    preparationBufferVertex :: TypedBuffer (V2 Float),
    preparationBufferIndex :: TypedBuffer (V3 Word32),
    preparationBufferUniform :: TypedBuffer (V2 Float)
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

    preparationBufferVertex <- newTypedBufferF Renderer {..}
        Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT
        [V2 0 0, V2 200 0, V2 200 200, V2 0 200]

    preparationBufferIndex <- newTypedBufferF Renderer {..}
        Vk.BUFFER_USAGE_INDEX_BUFFER_BIT
        [V3 0 1 2, V3 0 2 3]

    preparationBufferUniform <- newTypedBufferN Renderer {..} Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT 1

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
                Vk.buffer = typedBufferObject preparationBufferUniform,
                Vk.offset = 0,
                Vk.range = Vk.WHOLE_SIZE
            }
        } )
        -- copies
        V.empty

    pure Preparation {..}

freePreparation :: (MonadIO m) => Renderer -> Preparation -> m ()
freePreparation Renderer {..} Preparation {..} = do
    let Environment {..} = rendererEnvironment
    freeTypedBuffer Renderer {..} preparationBufferUniform
    freeTypedBuffer Renderer {..} preparationBufferIndex
    freeTypedBuffer Renderer {..} preparationBufferVertex
    Vk.freeDescriptorSets environmentDevice rendererDescriptorPool $ V.singleton preparationDescriptorSet
    Vk.freeCommandBuffers environmentDevice rendererCommandPool $ V.singleton preparationCommandBuffer

recordPreparation :: (MonadIO m) => Vk.CommandBuffer -> Renderer -> Preparation -> m ()
recordPreparation cmdbuf Renderer {..} Preparation {..} = do
    Vk.cmdBindPipeline cmdbuf Vk.PIPELINE_BIND_POINT_GRAPHICS rendererPipeline
    Vk.cmdBindIndexBuffer cmdbuf (typedBufferObject preparationBufferIndex) 0 Vk.INDEX_TYPE_UINT32

    Vk.cmdBindVertexBuffers cmdbuf 0
        (V.singleton $ typedBufferObject preparationBufferVertex)
        (V.singleton 0)

    Vk.cmdBindDescriptorSets cmdbuf
        Vk.PIPELINE_BIND_POINT_GRAPHICS
        rendererPipelineLayout
        0
        (V.singleton preparationDescriptorSet)
        (V.empty)

    Vk.cmdDrawIndexed preparationCommandBuffer 6 1 0 0 0

