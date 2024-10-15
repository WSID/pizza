{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}

module Graphics.Pizza.Preparation where

import Control.Monad.IO.Class

import Data.Word

import Foreign.Storable

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

data PreparationLinear = PreparationLinear {
    patternPosStart :: V2 Float,
    patternPosEnd :: V2 Float,
    patternColorStart :: V4 Float,
    patternColorEnd :: V4 Float
}

instance Storable PreparationLinear where
    sizeOf _ = 48
    alignment _ = 8

    peek ptr = do
        patternPosStart <- peekByteOff ptr 0
        patternPosEnd <- peekByteOff ptr 8
        patternColorStart <- peekByteOff ptr 16
        patternColorEnd <- peekByteOff ptr 32
        pure PreparationLinear {..}

    poke ptr PreparationLinear {..} = do
        pokeByteOff ptr 0 patternPosStart
        pokeByteOff ptr 8 patternPosEnd
        pokeByteOff ptr 16 patternColorStart
        pokeByteOff ptr 32 patternColorEnd


data PreparationRadial = PreparationRadial {
    patternPosCenter :: V2 Float,
    patternPosRadius :: Float,
    patternColorStart :: V4 Float,
    patternColorEnd :: V4 Float
}

instance Storable PreparationRadial where
    sizeOf _ = 48
    alignment _ = 8

    peek ptr = do
        patternPosCenter <- peekByteOff ptr 0
        patternPosRadius <- peekByteOff ptr 8
        patternColorStart <- peekByteOff ptr 16
        patternColorEnd <- peekByteOff ptr 32
        pure PreparationRadial {..}

    poke ptr PreparationRadial {..} = do
        pokeByteOff ptr 0 patternPosCenter
        pokeByteOff ptr 8 patternPosRadius
        pokeByteOff ptr 16 patternColorStart
        pokeByteOff ptr 32 patternColorEnd


-- | Rendering Operation
data Preparation = Preparation {
    preparationBufferVertex :: TypedBuffer (V2 Float),
    preparationBufferIndex :: TypedBuffer (V3 Word32),
    preparationPatternDS :: Vk.DescriptorSet,
    preparationBufferPattern :: TypedBuffer (),
    preparationPattern :: Pattern
}

newPreparation :: (MonadIO m) => Renderer -> Pattern -> m Preparation
newPreparation Renderer {..} pattern = do
    let Environment {..} = rendererEnvironment
        preparationPattern = pattern

    preparationBufferVertex <- newTypedBufferF Renderer {..}
        Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT
        [V2 0 0, V2 200 0, V2 200 200, V2 0 200]

    preparationBufferIndex <- newTypedBufferF Renderer {..}
        Vk.BUFFER_USAGE_INDEX_BUFFER_BIT
        [V3 0 1 2, V3 0 2 3]

    descriptorSets <- Vk.allocateDescriptorSets
        environmentDevice
        Vk.zero { -- Vk.DescriptorSetAllocateInfo
            Vk.next = (),
            Vk.descriptorPool = rendererDescriptorPool,
            Vk.setLayouts = V.singleton rendererPatternDSLayout
        }
    let preparationPatternDS = V.head descriptorSets

    preparationBufferPattern <- case pattern of
        PatternSolid color -> do
            patternBuffer <- newTypedBufferN Renderer {..} Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT 1
            writeTypedBuffer1 Renderer {..} patternBuffer color
            pure $ castTypedBuffer patternBuffer

        PatternLinear ps pe cs ce -> do
            patternBuffer <- newTypedBufferN Renderer {..} Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT 1
            writeTypedBuffer1 Renderer {..} patternBuffer $ PreparationLinear ps pe cs ce
            pure $ castTypedBuffer patternBuffer

        PatternRadial ps r cs ce -> do
            patternBuffer <- newTypedBufferN Renderer {..} Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT 1
            writeTypedBuffer1 Renderer {..} patternBuffer $ PreparationRadial ps r cs ce
            pure $ castTypedBuffer patternBuffer

    -- Fill content

    Vk.updateDescriptorSets
        environmentDevice
        -- writes
        (V.fromList [
            Vk.SomeStruct Vk.zero {
                Vk.next = (),
                Vk.dstSet = preparationPatternDS,
                Vk.dstBinding = 0,
                Vk.dstArrayElement = 0,
                Vk.descriptorCount = 1,
                Vk.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                Vk.bufferInfo = V.singleton $ Vk.zero {
                    Vk.buffer = typedBufferObject preparationBufferPattern,
                    Vk.offset = 0,
                    Vk.range = Vk.WHOLE_SIZE
                }
            }
        ] )
        -- copies
        V.empty

    pure Preparation {..}

freePreparation :: (MonadIO m) => Renderer -> Preparation -> m ()
freePreparation Renderer {..} Preparation {..} = do
    let Environment {..} = rendererEnvironment
    freeTypedBuffer Renderer {..} preparationBufferPattern
    freeTypedBuffer Renderer {..} preparationBufferIndex
    freeTypedBuffer Renderer {..} preparationBufferVertex
    Vk.freeDescriptorSets environmentDevice rendererDescriptorPool $ V.singleton preparationPatternDS

recordPreparationCommand :: (MonadIO m) => Renderer -> Preparation -> Vk.CommandBuffer -> m ()
recordPreparationCommand Renderer {..} Preparation {..} cmdbuf = do
    Vk.cmdBindPipeline
        cmdbuf
        Vk.PIPELINE_BIND_POINT_GRAPHICS
        (case preparationPattern of
            PatternSolid _ -> rendererPatternSolid
            PatternLinear {} -> rendererPatternLinear
            PatternRadial {} -> rendererPatternRadial
        )

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
        rendererPatternLayout
        1
        (V.singleton preparationPatternDS)
        (V.empty)

    Vk.cmdDrawIndexed cmdbuf 6 1 0 0 0
