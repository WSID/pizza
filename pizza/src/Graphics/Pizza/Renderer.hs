{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Graphics.Pizza.Renderer where

import Control.Monad.IO.Class

import Data.Bits
import Data.Traversable
import Data.Word

import Foreign.Storable
import Foreign.Ptr

-- linear
import Linear

-- vector
import qualified Data.Vector as V

-- bytestring
import qualified Data.ByteString.Char8 as BSC

-- vulkan
import qualified Vulkan as Vk
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Dynamic as Vk
import qualified Vulkan.Zero as Vk

-- vulkan utils
import qualified Vulkan.Utils.ShaderQQ.GLSL.Shaderc as Vku

-- VulkanMemoryAllocator
import qualified VulkanMemoryAllocator as Vma

-- pizza
import Graphics.Pizza.Environment


data Renderer = Renderer {
    -- Unowned reference
    rendererEnvironment :: Environment,

    -- Renderer Properties
    rendererImageFormat :: Vk.Format,
    rendererImageLayout :: Vk.ImageLayout,

    -- Stencil Render Pass
    -- TOOD: Check we can merge two as single render pass.
    rendererStencilRenderPass :: Vk.RenderPass,
    rendererShaderVert :: Vk.ShaderModule,
    rendererScreenDSLayout :: Vk.DescriptorSetLayout,

    -- Stencil Pipeline
    rendererStencilPipelineLayout :: Vk.PipelineLayout,
    rendererStencilPipeline :: Vk.Pipeline,

    -- Pattern Render Pass
    rendererRenderPass :: Vk.RenderPass,

    -- Pattern pipeline common
    rendererPatternDSLayout :: Vk.DescriptorSetLayout,
    rendererPatternLayout :: Vk.PipelineLayout,

    -- Pattern Solid
    rendererPatternSolidShaderFrag :: Vk.ShaderModule,
    rendererPatternSolid :: Vk.Pipeline,

    -- Pattern Linear
    rendererPatternLinearShaderFrag :: Vk.ShaderModule,
    rendererPatternLinear :: Vk.Pipeline,

    rendererPatternRadialShaderFrag :: Vk.ShaderModule,
    rendererPatternRadial :: Vk.Pipeline,

    -- Pools
    rendererDescriptorPool :: Vk.DescriptorPool,
    rendererCommandPool :: Vk.CommandPool
}

newRenderer :: (MonadIO m) => Environment -> Vk.Format -> Vk.ImageLayout -> m Renderer
newRenderer rendererEnvironment rendererImageFormat rendererImageLayout = do
    let Environment {..} = rendererEnvironment

    rendererStencilRenderPass <- Vk.createRenderPass
        environmentDevice
        Vk.zero { -- Vk.RenderPassCreateInfo
            Vk.attachments = V.singleton (Vk.zero :: Vk.AttachmentDescription) {
                Vk.format = Vk.FORMAT_S8_UINT,
                Vk.samples = Vk.SAMPLE_COUNT_1_BIT,
                Vk.loadOp = Vk.ATTACHMENT_LOAD_OP_DONT_CARE,
                Vk.storeOp = Vk.ATTACHMENT_STORE_OP_DONT_CARE,
                Vk.stencilLoadOp = Vk.ATTACHMENT_LOAD_OP_CLEAR,
                Vk.stencilStoreOp = Vk.ATTACHMENT_STORE_OP_STORE,
                Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED,
                Vk.finalLayout = Vk.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL
            },
            Vk.subpasses = V.singleton (Vk.zero :: Vk.SubpassDescription) {
                Vk.pipelineBindPoint = Vk.PIPELINE_BIND_POINT_GRAPHICS,
                Vk.depthStencilAttachment = Just Vk.AttachmentReference {
                    Vk.attachment = 0,
                    Vk.layout = Vk.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                }
            },
            Vk.dependencies = V.singleton (Vk.zero :: Vk.SubpassDependency) {
                Vk.srcSubpass = Vk.SUBPASS_EXTERNAL,
                Vk.srcStageMask = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
                Vk.dstStageMask = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
                Vk.dstAccessMask = Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
            }
        }
        Nothing

    rendererShaderVert <- Vk.createShaderModule
        environmentDevice
        Vk.zero { -- Vk.ShaderModuleCreateInfo
            Vk.code = [Vku.vert|
                #version 450

                layout (location = 0)
                in vec2 pos;

                layout (location = 0)
                out vec2 fragPos;

                layout (set = 0, binding = 0) uniform Screen {
                    vec2 size;
                };

                void main () {
                    fragPos = pos;

                    // Map (0, 0) ~ screenSize, to (-1, -1) ~ (+1, +1)
                    vec2 normPos = (pos / size * 2) - 1;

                    gl_Position = vec4(normPos, 0.0, 1.0);
                }
            |]
        }
        Nothing

    rendererScreenDSLayout <- Vk.createDescriptorSetLayout
        environmentDevice
        Vk.zero {
            Vk.bindings = V.singleton Vk.zero {
                Vk.binding = 0,
                Vk.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                Vk.descriptorCount = 1,
                Vk.stageFlags = Vk.SHADER_STAGE_VERTEX_BIT
            }
        }
        Nothing


    rendererStencilPipelineLayout <- Vk.createPipelineLayout
        environmentDevice
        Vk.PipelineLayoutCreateInfo {
            Vk.flags = zeroBits,
            Vk.setLayouts = V.singleton rendererScreenDSLayout,
            Vk.pushConstantRanges = V.empty
        }
        Nothing

    let stencilPipelineCreateInfo = (Vk.zero :: Vk.GraphicsPipelineCreateInfo '[]) {
            Vk.next = (),
            Vk.stageCount = 1,
            Vk.stages = V.singleton (
                Vk.SomeStruct Vk.zero {
                    Vk.stage = Vk.SHADER_STAGE_VERTEX_BIT,
                    Vk.name = BSC.pack "main",
                    Vk.module' = rendererShaderVert
                }
            ),

            Vk.vertexInputState = Just $ Vk.SomeStruct $ Vk.zero {
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
            },

            Vk.inputAssemblyState = Just $ Vk.zero {
                Vk.topology = Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
            },

            Vk.viewportState = Just $ Vk.SomeStruct Vk.zero {
                Vk.viewportCount = 1,
                Vk.scissorCount = 1
            },

            Vk.rasterizationState = Just $ Vk.SomeStruct Vk.zero {
                Vk.polygonMode = Vk.POLYGON_MODE_FILL,
                Vk.cullMode = Vk.CULL_MODE_BACK_BIT,
                Vk.frontFace = Vk.FRONT_FACE_CLOCKWISE,
                Vk.lineWidth = 1
            },

            Vk.multisampleState = Just $ Vk.SomeStruct Vk.zero {
                Vk.next = (),
                Vk.rasterizationSamples = Vk.SAMPLE_COUNT_1_BIT
            },

            Vk.depthStencilState = Just Vk.zero {
                Vk.stencilTestEnable = True,
                Vk.front = Vk.zero {
                    Vk.passOp = Vk.STENCIL_OP_INCREMENT_AND_WRAP,
                    Vk.compareOp = Vk.COMPARE_OP_ALWAYS,
                    Vk.compareMask = oneBits,
                    Vk.writeMask = oneBits
                },
                Vk.back = Vk.zero {
                    Vk.passOp = Vk.STENCIL_OP_DECREMENT_AND_WRAP,
                    Vk.compareOp = Vk.COMPARE_OP_ALWAYS,
                    Vk.compareMask = oneBits,
                    Vk.writeMask = oneBits
                }
            },

            Vk.colorBlendState = Nothing,

            Vk.dynamicState = Just Vk.zero {
                Vk.dynamicStates = V.fromList [
                    Vk.DYNAMIC_STATE_VIEWPORT,
                    Vk.DYNAMIC_STATE_SCISSOR
                ]
            },

            Vk.layout = rendererStencilPipelineLayout,
            Vk.renderPass = rendererStencilRenderPass,
            Vk.subpass = 0
        }

    (_, stencilPipelines) <- Vk.createGraphicsPipelines
        environmentDevice
        Vk.NULL_HANDLE
        (V.singleton $ Vk.SomeStruct stencilPipelineCreateInfo)
        Nothing

    let rendererStencilPipeline = V.head stencilPipelines

    rendererRenderPass <- Vk.createRenderPass
        environmentDevice
        Vk.zero {   -- Vk.RenderPassCreateInfo
            Vk.attachments = V.singleton (Vk.zero :: Vk.AttachmentDescription) {
                Vk.format = rendererImageFormat,
                Vk.samples = Vk.SAMPLE_COUNT_1_BIT,
                Vk.loadOp = Vk.ATTACHMENT_LOAD_OP_CLEAR,
                Vk.storeOp = Vk.ATTACHMENT_STORE_OP_STORE,
                Vk.stencilLoadOp = Vk.ATTACHMENT_LOAD_OP_DONT_CARE,
                Vk.stencilStoreOp = Vk.ATTACHMENT_STORE_OP_DONT_CARE,
                Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED,
                Vk.finalLayout = rendererImageLayout
            },
            Vk.subpasses = V.singleton (Vk.zero :: Vk.SubpassDescription) {
                Vk.pipelineBindPoint = Vk.PIPELINE_BIND_POINT_GRAPHICS,
                Vk.colorAttachments = V.singleton Vk.AttachmentReference {
                    Vk.attachment = 0,
                    Vk.layout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                }
            },
            Vk.dependencies = V.singleton (Vk.zero :: Vk.SubpassDependency) {
                Vk.srcSubpass = Vk.SUBPASS_EXTERNAL,
                Vk.srcStageMask = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
                Vk.dstStageMask = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
                Vk.dstAccessMask = Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
            }
        }
        Nothing

    let basePipelineCreateInfo fragShaderModule pipelineLayout = Vk.zero {
            Vk.next = (),
            Vk.stageCount = 2,
            Vk.stages = V.fromList [
                Vk.SomeStruct Vk.zero {
                    Vk.stage = Vk.SHADER_STAGE_VERTEX_BIT,
                    Vk.name = BSC.pack "main",
                    Vk.module' = rendererShaderVert
                },
                Vk.SomeStruct Vk.zero {
                    Vk.stage = Vk.SHADER_STAGE_FRAGMENT_BIT,
                    Vk.name = BSC.pack "main",
                    Vk.module' = fragShaderModule
                }
            ],

            Vk.vertexInputState = Just $ Vk.SomeStruct $ Vk.zero {
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
            },

            Vk.inputAssemblyState = Just $ Vk.zero {
                Vk.topology = Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
            },

            Vk.viewportState = Just $ Vk.SomeStruct Vk.zero {
                Vk.viewportCount = 1,
                Vk.scissorCount = 1
            },

            Vk.rasterizationState = Just $ Vk.SomeStruct Vk.zero {
                Vk.polygonMode = Vk.POLYGON_MODE_FILL,
                Vk.cullMode = Vk.CULL_MODE_BACK_BIT,
                Vk.frontFace = Vk.FRONT_FACE_CLOCKWISE,
                Vk.lineWidth = 1
            },

            Vk.multisampleState = Just $ Vk.SomeStruct Vk.zero {
                Vk.next = (),
                Vk.rasterizationSamples = Vk.SAMPLE_COUNT_1_BIT
            },

            Vk.depthStencilState = Nothing,

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
            Vk.renderPass = rendererRenderPass,
            Vk.subpass = 0
        }

    rendererPatternDSLayout <- Vk.createDescriptorSetLayout
        environmentDevice
        Vk.zero {
            Vk.bindings = V.singleton Vk.zero {
                Vk.binding = 0,
                Vk.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                Vk.descriptorCount = 1,
                Vk.stageFlags = Vk.SHADER_STAGE_FRAGMENT_BIT
            }
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


    rendererPatternSolidShaderFrag <- Vk.createShaderModule
        environmentDevice
        Vk.zero { -- Vk.ShaderModuleCreateInfo
            Vk.code = [Vku.frag|
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
        }
        Nothing

    rendererPatternLinearShaderFrag <- Vk.createShaderModule
        environmentDevice
        Vk.zero { -- Vk.ShaderModuleCreateInfo
            Vk.code = [Vku.frag|
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
        }
        Nothing

    rendererPatternRadialShaderFrag <- Vk.createShaderModule
        environmentDevice
        Vk.zero { -- Vk.ShaderModuleCreateInfo
            Vk.code = [Vku.frag|
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
        }
        Nothing


    (_, pipelines) <- Vk.createGraphicsPipelines
        environmentDevice
        Vk.NULL_HANDLE
        (V.fromList [
            Vk.SomeStruct $ basePipelineCreateInfo rendererPatternSolidShaderFrag rendererPatternLayout,
            Vk.SomeStruct $ basePipelineCreateInfo rendererPatternLinearShaderFrag rendererPatternLayout,
            Vk.SomeStruct $ basePipelineCreateInfo rendererPatternRadialShaderFrag rendererPatternLayout
        ] )
        Nothing
        -- TOOD: Move else to here!

    let [rendererPatternSolid, rendererPatternLinear, rendererPatternRadial] = V.toList pipelines

    rendererDescriptorPool <- Vk.createDescriptorPool
        environmentDevice
        Vk.zero { -- Vk.DescriptorPoolCreateInfo
            Vk.flags = Vk.DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT,
            Vk.maxSets = 32,
            Vk.poolSizes = V.singleton $
                Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER 32
        }
        Nothing

    rendererCommandPool <- Vk.createCommandPool
        environmentDevice
        Vk.CommandPoolCreateInfo { -- Vk.CommandPoolCreateInfo
            Vk.flags = Vk.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
            Vk.queueFamilyIndex = environmentGraphicsQFI
        }
        Nothing

    pure Renderer {..}


freeRenderer :: (MonadIO m) => Renderer -> m ()
freeRenderer Renderer {..} = do
    let Environment {..} = rendererEnvironment
    Vk.destroyPipeline environmentDevice rendererPatternRadial Nothing
    Vk.destroyShaderModule environmentDevice rendererPatternRadialShaderFrag Nothing
    Vk.destroyPipeline environmentDevice rendererPatternLinear Nothing
    Vk.destroyShaderModule environmentDevice rendererPatternLinearShaderFrag Nothing
    Vk.destroyPipeline environmentDevice rendererPatternSolid Nothing
    Vk.destroyShaderModule environmentDevice rendererPatternSolidShaderFrag Nothing
    Vk.destroyPipelineLayout environmentDevice rendererPatternLayout Nothing
    Vk.destroyDescriptorSetLayout environmentDevice rendererPatternDSLayout Nothing
    Vk.destroyRenderPass environmentDevice rendererRenderPass Nothing
    Vk.destroyPipeline environmentDevice rendererStencilPipeline Nothing
    Vk.destroyPipelineLayout environmentDevice rendererStencilPipelineLayout Nothing
    Vk.destroyDescriptorSetLayout environmentDevice rendererScreenDSLayout Nothing
    Vk.destroyShaderModule environmentDevice rendererShaderVert Nothing
    Vk.destroyRenderPass environmentDevice rendererStencilRenderPass Nothing
    Vk.destroyCommandPool environmentDevice rendererCommandPool Nothing
    Vk.destroyDescriptorPool environmentDevice rendererDescriptorPool Nothing
