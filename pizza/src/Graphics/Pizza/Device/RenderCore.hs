{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Graphics.Pizza.Device.RenderCore where

import Control.Monad.IO.Class

import Data.Bits
import Data.Word


-- linear
import Linear

-- vector
import qualified Data.Vector as V

-- bytestring
import qualified Data.ByteString.Char8 as BSC

-- vulkan
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk

-- vulkan utils
import qualified Vulkan.Utils.ShaderQQ.GLSL.Shaderc as Vku

-- pizza
import Graphics.Pizza.Device.Environment
import Graphics.Pizza.Internal.TypedBuffer

-- | A renderer context object.
--
data RenderCore = RenderCore {
    -- Unowned reference
    renderCoreEnvironment :: Environment,

    -- Vulkan Limits values needed for Renderer
    renderCoreMinUniformBufferOffsetAlign :: Word64,
    
    -- Pools
    renderCoreDescriptorPool :: Vk.DescriptorPool,
    renderCoreCommandPool :: Vk.CommandPool,

    -- Descriptor Set Layout
    renderCoreScreenDSLayout :: Vk.DescriptorSetLayout,
    renderCorePatternDSLayout :: Vk.DescriptorSetLayout,
    renderCorePatternImageDSLayout :: Vk.DescriptorSetLayout,

    -- Pipeline Layouts
    renderCoreStencilPipelineLayout :: Vk.PipelineLayout,
    renderCorePatternLayout :: Vk.PipelineLayout,

    -- Stencil Shader
    renderCoreStencilShaderVert :: Vk.ShaderModule,
    
    -- Pattern Pipeline Shaders
    renderCorePatternShaderVert :: Vk.ShaderModule,

    renderCorePatternSolidShaderFrag :: Vk.ShaderModule,
    renderCorePatternLinearShaderFrag :: Vk.ShaderModule,
    renderCorePatternRadialShaderFrag :: Vk.ShaderModule,
    renderCorePatternImageShaderFrag :: Vk.ShaderModule,

    -- Common Parts
    renderCoreQuadVertices :: TypedBuffer (V2 Float),
    renderCoreQuadIndices :: TypedBuffer (V3 Word32)
}

newRenderCore :: (MonadIO m) => Environment -> m RenderCore
newRenderCore renderCoreEnvironment = do
    let Environment {..} = renderCoreEnvironment

    -- Vulkan Limits

    Vk.PhysicalDeviceProperties {
        Vk.limits = Vk.PhysicalDeviceLimits {
            minUniformBufferOffsetAlignment = renderCoreMinUniformBufferOffsetAlign
        }
    } <- Vk.getPhysicalDeviceProperties environmentPhysDevice


    -- Descriptor Pool

    renderCoreDescriptorPool <- Vk.createDescriptorPool
        environmentDevice
        Vk.zero { -- Vk.DescriptorPoolCreateInfo
            Vk.flags = Vk.DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT,
            Vk.maxSets = 128,
            Vk.poolSizes = V.fromList [
                    Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER 32,
                    Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC 32,
                    Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER 128
                ]
        }
        Nothing

    -- Command Pool

    renderCoreCommandPool <- Vk.createCommandPool
        environmentDevice
        Vk.CommandPoolCreateInfo { -- Vk.CommandPoolCreateInfo
            Vk.flags = Vk.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
            Vk.queueFamilyIndex = environmentGraphicsQFI
        }
        Nothing


    -- Layouts : Descriptor Set Layouts

    renderCoreScreenDSLayout <- Vk.createDescriptorSetLayout
        environmentDevice
        Vk.zero {
            Vk.bindings = V.singleton Vk.zero {
                Vk.binding = 0,
                Vk.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                Vk.descriptorCount = 1,
                Vk.stageFlags = Vk.SHADER_STAGE_VERTEX_BIT .|. Vk.SHADER_STAGE_FRAGMENT_BIT
            }
        }
        Nothing

    renderCorePatternDSLayout <- Vk.createDescriptorSetLayout
        environmentDevice
        Vk.zero {
            Vk.bindings = V.fromList [
                Vk.zero {
                    Vk.binding = 0,
                    Vk.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC,
                    Vk.descriptorCount = 1,
                    Vk.stageFlags = Vk.SHADER_STAGE_FRAGMENT_BIT
                },
                Vk.zero {
                    Vk.binding = 1,
                    Vk.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC,
                    Vk.descriptorCount = 1,
                    Vk.stageFlags = Vk.SHADER_STAGE_VERTEX_BIT .|. Vk.SHADER_STAGE_FRAGMENT_BIT
                }
            ]
        }
        Nothing
    
    renderCorePatternImageDSLayout <- Vk.createDescriptorSetLayout
        environmentDevice
        Vk.zero {
            Vk.bindings = V.singleton (
                Vk.zero {
                    Vk.binding = 0,
                    Vk.descriptorType = Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                    Vk.descriptorCount = 1,
                    Vk.stageFlags = Vk.SHADER_STAGE_FRAGMENT_BIT
                }
            )
        }
        Nothing

    -- Layouts : Pipeline Layouts

    renderCoreStencilPipelineLayout <- Vk.createPipelineLayout
        environmentDevice
        Vk.PipelineLayoutCreateInfo {
            Vk.flags = zeroBits,
            Vk.setLayouts = V.singleton renderCoreScreenDSLayout,
            Vk.pushConstantRanges = V.empty
        }
        Nothing

    renderCorePatternLayout <- Vk.createPipelineLayout
        environmentDevice
        Vk.PipelineLayoutCreateInfo {
            Vk.flags = zeroBits,
            Vk.setLayouts = V.fromList [
                renderCoreScreenDSLayout,
                renderCorePatternDSLayout,
                renderCorePatternImageDSLayout
            ],
            Vk.pushConstantRanges = V.empty
        }
        Nothing

    -- Vertex Shader Module

    renderCoreStencilShaderVert <- Vk.createShaderModule
        environmentDevice
        Vk.ShaderModuleCreateInfo {
            Vk.next = (),
            Vk.flags = zeroBits,
            Vk.code = codeStencilVert
        }
        Nothing

    -- Pipeline : Patterns Pipeline

    renderCorePatternShaderVert <- Vk.createShaderModule
        environmentDevice
        Vk.ShaderModuleCreateInfo {
            Vk.next = (),
            Vk.flags = zeroBits,
            Vk.code = codePatternVert
        }
        Nothing

    renderCorePatternSolidShaderFrag <- Vk.createShaderModule
        environmentDevice
        Vk.ShaderModuleCreateInfo {
            Vk.next = (),
            Vk.flags = zeroBits,
            Vk.code = codePatternSolidFrag
        }
        Nothing

    renderCorePatternLinearShaderFrag <- Vk.createShaderModule
        environmentDevice
        Vk.ShaderModuleCreateInfo {
            Vk.next = (),
            Vk.flags = zeroBits,
            Vk.code = codePatternLinearFrag
        }
        Nothing

    renderCorePatternRadialShaderFrag <- Vk.createShaderModule
        environmentDevice
        Vk.ShaderModuleCreateInfo {
            Vk.next = (),
            Vk.flags = zeroBits,
            Vk.code = codePatternRadialFrag
        }
        Nothing

    renderCorePatternImageShaderFrag <- Vk.createShaderModule
        environmentDevice
        Vk.ShaderModuleCreateInfo {
            Vk.next = (),
            Vk.flags = zeroBits,
            Vk.code = codePatternImageFrag
        }
        Nothing


    -- Basic Shapes

    renderCoreQuadVertices <- newTypedBufferF
        renderCoreEnvironment
        Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT
        [
            V2 (-1) (-1),
            V2 1 (-1),
            V2 1 1,
            V2 (-1) 1
        ]

    renderCoreQuadIndices <- newTypedBufferF
        renderCoreEnvironment
        Vk.BUFFER_USAGE_INDEX_BUFFER_BIT
        [
            V3 0 1 2,
            V3 0 2 3
        ]
    
    pure RenderCore {..}

freeRenderCore :: (MonadIO m) => RenderCore -> m ()
freeRenderCore RenderCore {..} = do
    let Environment {..} = renderCoreEnvironment

    freeTypedBuffer renderCoreEnvironment renderCoreQuadVertices
    freeTypedBuffer renderCoreEnvironment renderCoreQuadIndices

    Vk.destroyShaderModule environmentDevice renderCorePatternImageShaderFrag Nothing
    Vk.destroyShaderModule environmentDevice renderCorePatternRadialShaderFrag Nothing
    Vk.destroyShaderModule environmentDevice renderCorePatternLinearShaderFrag Nothing
    Vk.destroyShaderModule environmentDevice renderCorePatternSolidShaderFrag Nothing
    Vk.destroyShaderModule environmentDevice renderCorePatternShaderVert Nothing

    Vk.destroyShaderModule environmentDevice renderCoreStencilShaderVert Nothing

    Vk.destroyPipelineLayout environmentDevice renderCorePatternLayout Nothing
    Vk.destroyPipelineLayout environmentDevice renderCoreStencilPipelineLayout Nothing
    Vk.destroyDescriptorSetLayout environmentDevice renderCorePatternImageDSLayout Nothing
    Vk.destroyDescriptorSetLayout environmentDevice renderCorePatternDSLayout Nothing
    Vk.destroyDescriptorSetLayout environmentDevice renderCoreScreenDSLayout Nothing

    Vk.destroyCommandPool environmentDevice renderCoreCommandPool Nothing
    Vk.destroyDescriptorPool environmentDevice renderCoreDescriptorPool Nothing


-- Shader code

codeStencilVert :: BSC.ByteString
codeStencilVert = [Vku.vert|
        #version 450

        layout (location = 0)
        in vec2 pos;

        layout (set = 0, binding = 0) uniform Screen {
            vec2 size;
        };

        void main () {
            // Map (0, 0) ~ screenSize, to (-1, -1) ~ (+1, +1)
            vec2 normPos = (pos / size * 2) - 1;

            gl_Position = vec4(normPos, 0.0, 1.0);
        }
    |]

codePatternVert :: BSC.ByteString
codePatternVert = [Vku.vert|
        #version 450

        layout (location = 0)
        in vec2 pos;

        layout (location = 0)
        out vec2 fragPos;

        layout (set = 0, binding = 0) uniform Screen {
            vec2 size;
        };

        layout (set = 1, binding = 1) uniform Transform {
            mat2 transMatrix;
            vec2 transTrans;
        };

        void main () {
            gl_Position = vec4 (pos, 0, 1);

            // Map (-1, -1) ~ (+1, +1), to (0, 0) ~ screenSize
            fragPos = inverse(transMatrix) * (((pos + 1) * 0.5f * size) - transTrans);
        }
    |]

codePatternSolidFrag :: BSC.ByteString
codePatternSolidFrag = [Vku.frag|
        #version 450

        layout (location = 0)
        in vec2 fragPos;

        layout (location = 0)
        out vec4 color;

        layout (set = 1, binding = 0) uniform PatternSolid {
            vec4 patternColor;
        };

        void main () {
            color = patternColor;
        }
    |]

codePatternLinearFrag :: BSC.ByteString
codePatternLinearFrag = [Vku.frag|
        #version 450

        layout (location = 0)
        in vec2 fragPos;

        layout (location = 0)
        out vec4 color;

        layout (set = 1, binding = 0) uniform PatternLinear {
            vec2 patternPosStart;
            vec2 patternPosEnd;
            vec4 patternColorStart;
            vec4 patternColorEnd;
        };

        void main () {
            vec2 relPos = fragPos - patternPosStart;
            vec2 relEnd = patternPosEnd - patternPosStart;

            float alpha = clamp (dot(relPos, relEnd) / dot(relEnd, relEnd), 0, 1);
            color = mix (patternColorStart, patternColorEnd, alpha);
        }
    |]

codePatternRadialFrag :: BSC.ByteString
codePatternRadialFrag = [Vku.frag|
        #version 450

        layout (location = 0)
        in vec2 fragPos;

        layout (location = 0)
        out vec4 color;

        layout (set = 1, binding = 0) uniform PatternRadial {
            vec2 patternPosCenter;
            float patternPosRadius;
            vec4 patternColorStart;
            vec4 patternColorEnd;
        };

        void main () {
            float dist = distance(fragPos, patternPosCenter);

            float alpha = clamp (dist / patternPosRadius, 0, 1);
            color = mix (patternColorStart, patternColorEnd, alpha);
        }
    |]

codePatternImageFrag :: BSC.ByteString
codePatternImageFrag = [Vku.frag|
        #version 450

        layout (location = 0)
        in vec2 fragPos;

        layout (location = 0)
        out vec4 color;

        layout (set = 1, binding = 0) uniform PatternImage {
            mat2 imageTransMatrix;
            vec2 imageTransTrans;
            float imageOpacity;
        };

        layout (set = 2, binding = 0) uniform sampler2D image;

        void main () {
            vec2 texCoord = imageTransTrans + imageTransMatrix * fragPos;
            vec4 texColor = texture (image, texCoord);
            color = vec4(texColor.rgb, imageOpacity * texColor.a);
        }
    |]

