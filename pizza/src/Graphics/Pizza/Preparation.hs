{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}

module Graphics.Pizza.Preparation where

import Control.Monad.IO.Class

import Data.Word

-- linear
import Linear

-- vector
import qualified Data.Vector as V

-- vulkan
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import qualified Vulkan.CStruct.Extends as Vk

-- pizza
import Graphics.Pizza.Graphic
import Graphics.Pizza.Renderer
import Graphics.Pizza.Internal.TypedBuffer

-- | Rendering Operation
data Preparation = Preparation {
    preparationPatternSolidDS :: Vk.DescriptorSet,
    preparationBufferVertex :: TypedBuffer (V2 Float),
    preparationBufferIndex :: TypedBuffer (V3 Word32),
    preparationBufferUniform :: TypedBuffer (V4 Float)
}

newPreparation :: (MonadIO m) => Renderer -> Pattern -> m Preparation
newPreparation Renderer {..} pattern = do
    let Environment {..} = rendererEnvironment
    descriptorSets <- Vk.allocateDescriptorSets
        environmentDevice
        Vk.zero { -- Vk.DescriptorSetAllocateInfo
            Vk.next = (),
            Vk.descriptorPool = rendererDescriptorPool,
            Vk.setLayouts = V.singleton rendererPatternSolidDSLayout
        }
    let preparationPatternSolidDS = V.head descriptorSets

    preparationBufferVertex <- newTypedBufferF Renderer {..}
        Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT
        [V2 0 0, V2 200 0, V2 200 200, V2 0 200]

    preparationBufferIndex <- newTypedBufferF Renderer {..}
        Vk.BUFFER_USAGE_INDEX_BUFFER_BIT
        [V3 0 1 2, V3 0 2 3]

    preparationBufferUniform <- newTypedBufferN Renderer {..} Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT 1
    writeTypedBuffer1 Renderer {..} preparationBufferUniform $ case pattern of
        PatternSolid color -> color

    -- Fill content

    Vk.updateDescriptorSets
        environmentDevice
        -- writes
        (V.singleton $ Vk.SomeStruct Vk.zero {
            Vk.next = (),
            Vk.dstSet = preparationPatternSolidDS,
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
    Vk.freeDescriptorSets environmentDevice rendererDescriptorPool $ V.singleton preparationPatternSolidDS

recordPreparationCommand :: (MonadIO m) => Renderer -> Preparation -> Vk.CommandBuffer -> m ()
recordPreparationCommand Renderer {..} Preparation {..} cmdbuf = do
    Vk.cmdBindPipeline
        cmdbuf
        Vk.PIPELINE_BIND_POINT_GRAPHICS
        rendererPatternSolid

    Vk.cmdBindIndexBuffer
        cmdbuf
        (typedBufferObject preparationBufferIndex) 0 Vk.INDEX_TYPE_UINT32

    Vk.cmdBindVertexBuffers
        cmdbuf
        0
        (V.singleton $ typedBufferObject preparationBufferVertex)
        (V.singleton 0)

    Vk.cmdBindDescriptorSets
        cmdbuf
        Vk.PIPELINE_BIND_POINT_GRAPHICS
        rendererPatternSolidLayout
        1
        (V.singleton preparationPatternSolidDS)
        (V.empty)

    Vk.cmdDrawIndexed cmdbuf 6 1 0 0 0
