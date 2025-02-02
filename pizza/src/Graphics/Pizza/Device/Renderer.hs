{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Graphics.Pizza.Device.Renderer where

import Control.Monad.IO.Class

import Data.Bits
import Data.Word

import Foreign.Storable

-- linear
import Linear

-- vector
import qualified Data.Vector as V

-- bytestring
import qualified Data.ByteString.Char8 as BSC

-- vulkan
import qualified Vulkan as Vk
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Zero as Vk

-- vulkan utils
import qualified Vulkan.Utils.ShaderQQ.GLSL.Shaderc as Vku

-- pizza
import Graphics.Pizza.Device.Environment
import Graphics.Pizza.Device.Format
import Graphics.Pizza.Internal.TypedBuffer

-- | A renderer context object.
--
-- For type `Renderer px`...
--
--  px is a pixel format. Typically use `VRGBA (UNorm Word8)` for 8bit format.
data Renderer px = Renderer {
    -- Unowned reference
    rendererEnvironment :: Environment,

    -- Vulkan Limits values needed for Renderer
    rendererMinUniformBufferOffsetAlign :: Word64,

    -- Renderer Properties
    rendererImageFormat :: Vk.Format,
    rendererImageLayout :: Vk.ImageLayout,

    -- Descriptor Set Layout
    rendererScreenDSLayout :: Vk.DescriptorSetLayout,
    rendererPatternDSLayout :: Vk.DescriptorSetLayout,

    -- Pipeline Layouts
    rendererStencilPipelineLayout :: Vk.PipelineLayout,
    rendererPatternLayout :: Vk.PipelineLayout,

    -- Render Pass
    rendererRenderPass :: Vk.RenderPass,

    -- Stencil Pipeline
    rendererShaderVert :: Vk.ShaderModule,
    rendererStencilPipeline :: Vk.Pipeline,

    -- Pattern Pipeline Shaders
    rendererPatternShaderVert :: Vk.ShaderModule,

    rendererPatternSolidShaderFrag :: Vk.ShaderModule,
    rendererPatternLinearShaderFrag :: Vk.ShaderModule,
    rendererPatternRadialShaderFrag :: Vk.ShaderModule,

    -- Pattern Solid
    rendererPatternSolid :: Vk.Pipeline,
    rendererPatternLinear :: Vk.Pipeline,
    rendererPatternRadial :: Vk.Pipeline,

    -- Pools
    rendererDescriptorPool :: Vk.DescriptorPool,
    rendererCommandPool :: Vk.CommandPool,

    -- Common Parts
    rendererQuadVertices :: TypedBuffer (V2 Float),
    rendererQuadIndices :: TypedBuffer (V3 Word32)
}

newRenderer :: (MonadIO m, Format px) => Environment -> Vk.ImageLayout -> m (Renderer px)
newRenderer rendererEnvironment rendererImageLayout = newRendererOf rendererEnvironment rendererImageLayout undefined

newRendererOf :: (MonadIO m, Format px) => Environment -> Vk.ImageLayout -> px -> m (Renderer px)
newRendererOf rendererEnvironment rendererImageLayout px = do
    let Environment {..} = rendererEnvironment
        rendererImageFormat = formatOf px

    Vk.PhysicalDeviceProperties {
        Vk.limits = Vk.PhysicalDeviceLimits {
            minUniformBufferOffsetAlignment = rendererMinUniformBufferOffsetAlign
        }
    } <- Vk.getPhysicalDeviceProperties environmentPhysDevice

    -- Layouts : Descriptor Set Layouts

    rendererScreenDSLayout <- Vk.createDescriptorSetLayout
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

    rendererPatternDSLayout <- Vk.createDescriptorSetLayout
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

    -- Layouts : Pipeline Layouts

    rendererStencilPipelineLayout <- Vk.createPipelineLayout
        environmentDevice
        Vk.PipelineLayoutCreateInfo {
            Vk.flags = zeroBits,
            Vk.setLayouts = V.singleton rendererScreenDSLayout,
            Vk.pushConstantRanges = V.empty
        }
        Nothing

    rendererPatternLayout <- Vk.createPipelineLayout
        environmentDevice
        Vk.PipelineLayoutCreateInfo {
            Vk.flags = zeroBits,
            Vk.setLayouts = V.fromList [
                rendererScreenDSLayout,
                rendererPatternDSLayout
            ],
            Vk.pushConstantRanges = V.empty
        }
        Nothing

    -- Render Pass

    rendererRenderPass <- Vk.createRenderPass
        environmentDevice
        Vk.RenderPassCreateInfo {
            Vk.next = (),
            Vk.flags = zeroBits,
            Vk.attachments = V.fromList [
                -- Color!
                Vk.AttachmentDescription {
                    Vk.flags = zeroBits,
                    Vk.format = rendererImageFormat,
                    Vk.samples = Vk.SAMPLE_COUNT_1_BIT,
                    Vk.loadOp = Vk.ATTACHMENT_LOAD_OP_CLEAR,
                    Vk.storeOp = Vk.ATTACHMENT_STORE_OP_STORE,
                    Vk.stencilLoadOp = Vk.ATTACHMENT_LOAD_OP_DONT_CARE,
                    Vk.stencilStoreOp = Vk.ATTACHMENT_STORE_OP_DONT_CARE,
                    Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED,
                    Vk.finalLayout = rendererImageLayout
                },
                -- Stencil
                Vk.AttachmentDescription {
                    Vk.flags = zeroBits,
                    Vk.format = Vk.FORMAT_S8_UINT,
                    Vk.samples = Vk.SAMPLE_COUNT_1_BIT,
                    Vk.loadOp = Vk.ATTACHMENT_LOAD_OP_DONT_CARE,
                    Vk.storeOp = Vk.ATTACHMENT_STORE_OP_DONT_CARE,
                    Vk.stencilLoadOp = Vk.ATTACHMENT_LOAD_OP_CLEAR,
                    Vk.stencilStoreOp = Vk.ATTACHMENT_STORE_OP_DONT_CARE,
                    Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED,
                    Vk.finalLayout = Vk.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                }
            ],
            Vk.subpasses = V.singleton Vk.SubpassDescription {
                Vk.flags = zeroBits,
                Vk.pipelineBindPoint = Vk.PIPELINE_BIND_POINT_GRAPHICS,
                Vk.inputAttachments = V.empty,
                Vk.colorAttachments = V.singleton Vk.AttachmentReference {
                    Vk.attachment = 0,
                    Vk.layout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                },
                Vk.resolveAttachments = V.empty,
                Vk.depthStencilAttachment = Just Vk.AttachmentReference {
                    Vk.attachment = 1,
                    Vk.layout = Vk.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                },
                Vk.preserveAttachments = V.empty
            },
            Vk.dependencies = V.singleton Vk.SubpassDependency {
                Vk.srcSubpass = Vk.SUBPASS_EXTERNAL,
                Vk.dstSubpass = 0,
                Vk.srcStageMask = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
                Vk.dstStageMask = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
                Vk.srcAccessMask = zeroBits,
                Vk.dstAccessMask = Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT,
                Vk.dependencyFlags = zeroBits
            }
        }
        Nothing

    -- Pipeline : Stencil Pipeline

    rendererShaderVert <- Vk.createShaderModule
        environmentDevice
        Vk.ShaderModuleCreateInfo {
            Vk.next = (),
            Vk.flags = zeroBits,
            Vk.code = codeStencilVert
        }
        Nothing

    let stencilPipelineCreateInfo = baseStencilPipelineInfo
            rendererStencilPipelineLayout
            rendererRenderPass
            rendererShaderVert
            Vk.zero {
                Vk.stencilTestEnable = True,
                Vk.front = Vk.zero {
                    Vk.passOp = Vk.STENCIL_OP_INCREMENT_AND_WRAP,
                    Vk.compareOp = Vk.COMPARE_OP_ALWAYS,
                    Vk.compareMask = 0xFFFF,
                    Vk.writeMask = 0xFFFF
                },
                Vk.back = Vk.zero {
                    Vk.passOp = Vk.STENCIL_OP_DECREMENT_AND_WRAP,
                    Vk.compareOp = Vk.COMPARE_OP_ALWAYS,
                    Vk.compareMask = 0xFFFF,
                    Vk.writeMask = 0xFFFF
                }
            }

    (_, stencilPipelines) <- Vk.createGraphicsPipelines
        environmentDevice
        Vk.NULL_HANDLE
        (V.singleton $ Vk.SomeStruct stencilPipelineCreateInfo)
        Nothing

    let rendererStencilPipeline = V.head stencilPipelines

    -- Pipeline : Patterns Pipeline

    rendererPatternShaderVert <- Vk.createShaderModule
        environmentDevice
        Vk.ShaderModuleCreateInfo {
            Vk.next = (),
            Vk.flags = zeroBits,
            Vk.code = codePatternVert
        }
        Nothing

    rendererPatternSolidShaderFrag <- Vk.createShaderModule
        environmentDevice
        Vk.ShaderModuleCreateInfo {
            Vk.next = (),
            Vk.flags = zeroBits,
            Vk.code = codePatternSolidFrag
        }
        Nothing

    rendererPatternLinearShaderFrag <- Vk.createShaderModule
        environmentDevice
        Vk.ShaderModuleCreateInfo {
            Vk.next = (),
            Vk.flags = zeroBits,
            Vk.code = codePatternLinearFrag
        }
        Nothing

    rendererPatternRadialShaderFrag <- Vk.createShaderModule
        environmentDevice
        Vk.ShaderModuleCreateInfo {
            Vk.next = (),
            Vk.flags = zeroBits,
            Vk.code = codePatternRadialFrag
        }
        Nothing

    (_, pipelines) <- Vk.createGraphicsPipelines
        environmentDevice
        Vk.NULL_HANDLE
        (V.fromList [
            Vk.SomeStruct $ basePatternPipelineCreateInfo
                rendererPatternLayout
                rendererRenderPass
                rendererPatternShaderVert
                rendererPatternSolidShaderFrag,
            Vk.SomeStruct $ basePatternPipelineCreateInfo
                rendererPatternLayout
                rendererRenderPass
                rendererPatternShaderVert
                rendererPatternLinearShaderFrag,
            Vk.SomeStruct $ basePatternPipelineCreateInfo
                rendererPatternLayout
                rendererRenderPass
                rendererPatternShaderVert
                rendererPatternRadialShaderFrag
        ] )
        Nothing
        -- TOOD: Move else to here!


    let (rendererPatternSolid, rendererPatternLinear, rendererPatternRadial) =
            case V.toList pipelines of
                [a, b, c] -> (a, b, c)
                _ -> error "newRenderer: Got incorrect number of pipeline!"

    -- Descriptor Pool

    rendererDescriptorPool <- Vk.createDescriptorPool
        environmentDevice
        Vk.zero { -- Vk.DescriptorPoolCreateInfo
            Vk.flags = Vk.DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT,
            Vk.maxSets = 32,
            Vk.poolSizes = V.fromList [
                    Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER 32,
                    Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC 32
                ]
        }
        Nothing

    -- Command Pool

    rendererCommandPool <- Vk.createCommandPool
        environmentDevice
        Vk.CommandPoolCreateInfo { -- Vk.CommandPoolCreateInfo
            Vk.flags = Vk.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
            Vk.queueFamilyIndex = environmentGraphicsQFI
        }
        Nothing


    -- Basic Shapes

    rendererQuadVertices <- newTypedBufferF
        rendererEnvironment
        Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT
        [
            V2 (-1) (-1),
            V2 1 (-1),
            V2 1 1,
            V2 (-1) 1
        ]

    rendererQuadIndices <- newTypedBufferF
        rendererEnvironment
        Vk.BUFFER_USAGE_INDEX_BUFFER_BIT
        [
            V3 0 1 2,
            V3 0 2 3
        ]

    pure Renderer {..}


freeRenderer :: (MonadIO m) => Renderer px -> m ()
freeRenderer Renderer {..} = do
    let Environment {..} = rendererEnvironment
    freeTypedBuffer rendererEnvironment rendererQuadIndices
    freeTypedBuffer rendererEnvironment rendererQuadVertices

    Vk.destroyCommandPool environmentDevice rendererCommandPool Nothing
    Vk.destroyDescriptorPool environmentDevice rendererDescriptorPool Nothing

    Vk.destroyPipeline environmentDevice rendererPatternRadial Nothing
    Vk.destroyPipeline environmentDevice rendererPatternLinear Nothing
    Vk.destroyPipeline environmentDevice rendererPatternSolid Nothing

    Vk.destroyShaderModule environmentDevice rendererPatternRadialShaderFrag Nothing
    Vk.destroyShaderModule environmentDevice rendererPatternLinearShaderFrag Nothing
    Vk.destroyShaderModule environmentDevice rendererPatternSolidShaderFrag Nothing
    Vk.destroyShaderModule environmentDevice rendererPatternShaderVert Nothing

    Vk.destroyPipeline environmentDevice rendererStencilPipeline Nothing
    Vk.destroyShaderModule environmentDevice rendererShaderVert Nothing

    Vk.destroyRenderPass environmentDevice rendererRenderPass Nothing

    Vk.destroyPipelineLayout environmentDevice rendererPatternLayout Nothing
    Vk.destroyPipelineLayout environmentDevice rendererStencilPipelineLayout Nothing
    Vk.destroyDescriptorSetLayout environmentDevice rendererPatternDSLayout Nothing
    Vk.destroyDescriptorSetLayout environmentDevice rendererScreenDSLayout Nothing



-- Common part of pipeline build

baseVertexInputStateCreateInfo :: Vk.PipelineVertexInputStateCreateInfo '[]
baseVertexInputStateCreateInfo = Vk.PipelineVertexInputStateCreateInfo {
        Vk.next = (),
        Vk.flags = zeroBits,
        Vk.vertexBindingDescriptions = V.singleton Vk.VertexInputBindingDescription {
            Vk.binding = 0,
            Vk.stride = fromIntegral $ sizeOf (undefined :: V2 Float),
            Vk.inputRate = Vk.VERTEX_INPUT_RATE_VERTEX
        },
        Vk.vertexAttributeDescriptions = V.singleton Vk.VertexInputAttributeDescription {
            Vk.location = 0,
            Vk.binding = 0,
            Vk.format = Vk.FORMAT_R32G32_SFLOAT,
            Vk.offset = 0
        }
    }

baseInputAssemblyStateCreateInfo :: Vk.PipelineInputAssemblyStateCreateInfo
baseInputAssemblyStateCreateInfo = Vk.PipelineInputAssemblyStateCreateInfo {
        Vk.flags = zeroBits,
        Vk.topology = Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
        Vk.primitiveRestartEnable = False
    }

baseViewportStateCreateInfo :: Vk.PipelineViewportStateCreateInfo '[]
baseViewportStateCreateInfo = Vk.PipelineViewportStateCreateInfo {
        Vk.next = (),
        Vk.flags = zeroBits,
        Vk.viewportCount = 1,
        Vk.viewports = V.empty,
        Vk.scissorCount = 1,
        Vk.scissors = V.empty
    }

baseMultisampleStateCreateInfo :: Vk.PipelineMultisampleStateCreateInfo '[]
baseMultisampleStateCreateInfo = Vk.PipelineMultisampleStateCreateInfo {
        Vk.next = (),
        Vk.flags = zeroBits,
        Vk.rasterizationSamples = Vk.SAMPLE_COUNT_1_BIT,
        Vk.sampleShadingEnable = False,
        Vk.minSampleShading = 0.0,
        Vk.sampleMask = V.empty,
        Vk.alphaToCoverageEnable = False,
        Vk.alphaToOneEnable = False
    }


baseStencilPipelineInfo
    :: Vk.PipelineLayout
    -> Vk.RenderPass
    -> Vk.ShaderModule
    -> Vk.PipelineDepthStencilStateCreateInfo
    -> Vk.GraphicsPipelineCreateInfo '[]
baseStencilPipelineInfo pipelineLayout renderPass vertModule depthState = Vk.GraphicsPipelineCreateInfo {
        Vk.next = (),
        Vk.flags = zeroBits,
        Vk.stageCount = 1,
        Vk.stages = V.singleton (
            Vk.SomeStruct Vk.zero {
                Vk.stage = Vk.SHADER_STAGE_VERTEX_BIT,
                Vk.name = BSC.pack "main",
                Vk.module' = vertModule
            }
        ),

        Vk.vertexInputState = Just $ Vk.SomeStruct baseVertexInputStateCreateInfo,
        Vk.inputAssemblyState = Just baseInputAssemblyStateCreateInfo,
        Vk.viewportState = Just $ Vk.SomeStruct baseViewportStateCreateInfo,
        Vk.tessellationState = Nothing,
        Vk.rasterizationState = Just $ Vk.SomeStruct Vk.zero {
            Vk.polygonMode = Vk.POLYGON_MODE_FILL,
            Vk.cullMode = Vk.CULL_MODE_NONE,
            Vk.frontFace = Vk.FRONT_FACE_CLOCKWISE,
            Vk.lineWidth = 1
        },

        Vk.multisampleState = Just $ Vk.SomeStruct baseMultisampleStateCreateInfo,
        Vk.depthStencilState = Just depthState,

        Vk.colorBlendState = Just $ Vk.SomeStruct Vk.zero {
            Vk.logicOpEnable = False,
            Vk.attachmentCount = 1,
            Vk.attachments = V.fromList [
                Vk.zero {
                    Vk.colorWriteMask =
                        Vk.COLOR_COMPONENT_R_BIT .|.
                        Vk.COLOR_COMPONENT_G_BIT .|.
                        Vk.COLOR_COMPONENT_B_BIT .|.
                        Vk.COLOR_COMPONENT_A_BIT,
                    Vk.blendEnable = False
                }
            ]
        },

        Vk.dynamicState = Just Vk.zero {
            Vk.dynamicStates = V.fromList [
                Vk.DYNAMIC_STATE_VIEWPORT,
                Vk.DYNAMIC_STATE_SCISSOR
            ]
        },

        Vk.layout = pipelineLayout,
        Vk.renderPass = renderPass,
        Vk.subpass = 0,

        Vk.basePipelineHandle = Vk.NULL_HANDLE,
        Vk.basePipelineIndex = -1
    }


basePatternPipelineCreateInfo
    :: Vk.PipelineLayout
    -> Vk.RenderPass
    -> Vk.ShaderModule
    -> Vk.ShaderModule
    -> Vk.GraphicsPipelineCreateInfo '[]
basePatternPipelineCreateInfo pipelineLayout renderPass vertModule fragModule = Vk.zero {
        Vk.next = (),
        Vk.stageCount = 2,
        Vk.stages = V.fromList [
            Vk.SomeStruct Vk.zero {
                Vk.stage = Vk.SHADER_STAGE_VERTEX_BIT,
                Vk.name = BSC.pack "main",
                Vk.module' = vertModule
            },
            Vk.SomeStruct Vk.zero {
                Vk.stage = Vk.SHADER_STAGE_FRAGMENT_BIT,
                Vk.name = BSC.pack "main",
                Vk.module' = fragModule
            }
        ],

        Vk.vertexInputState = Just $ Vk.SomeStruct baseVertexInputStateCreateInfo,
        Vk.inputAssemblyState = Just baseInputAssemblyStateCreateInfo,
        Vk.viewportState = Just $ Vk.SomeStruct baseViewportStateCreateInfo,
        Vk.tessellationState = Nothing,

        Vk.rasterizationState = Just $ Vk.SomeStruct Vk.zero {
            Vk.polygonMode = Vk.POLYGON_MODE_FILL,
            Vk.cullMode = Vk.CULL_MODE_NONE,
            Vk.lineWidth = 1
        },

        Vk.multisampleState = Just $ Vk.SomeStruct baseMultisampleStateCreateInfo,

        -- Use even-odd rule here
        -- Just check first bit only.
        Vk.depthStencilState = Just Vk.zero {
            Vk.stencilTestEnable = True,
            Vk.front = Vk.zero {
                Vk.compareOp = Vk.COMPARE_OP_EQUAL,
                Vk.compareMask = 1,
                Vk.reference = 1
            },
            Vk.back = Vk.zero {
                Vk.compareOp = Vk.COMPARE_OP_EQUAL,
                Vk.compareMask = 1,
                Vk.reference = 1
            }
        },

        Vk.colorBlendState = Just $ Vk.SomeStruct Vk.zero {
            Vk.logicOpEnable = False,
            Vk.attachmentCount = 1,
            Vk.attachments = V.fromList [
                Vk.zero {
                    Vk.colorWriteMask =
                        Vk.COLOR_COMPONENT_R_BIT .|.
                        Vk.COLOR_COMPONENT_G_BIT .|.
                        Vk.COLOR_COMPONENT_B_BIT .|.
                        Vk.COLOR_COMPONENT_A_BIT,
                    Vk.blendEnable = True
                }
            ]
        },

        Vk.dynamicState = Just Vk.zero {
            Vk.dynamicStates = V.fromList [
                Vk.DYNAMIC_STATE_VIEWPORT,
                Vk.DYNAMIC_STATE_SCISSOR,
                Vk.DYNAMIC_STATE_COLOR_BLEND_ADVANCED_EXT
            ]
        },

        Vk.layout = pipelineLayout,
        Vk.renderPass = renderPass,
        Vk.subpass = 0
    }

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


