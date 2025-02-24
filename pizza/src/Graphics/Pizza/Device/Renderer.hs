{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}

module Graphics.Pizza.Device.Renderer where

import Control.Monad.IO.Class

import Data.Bits
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

-- pizza
import Graphics.Pizza.Device.Environment
import Graphics.Pizza.Device.Format
import Graphics.Pizza.Device.RenderCore

-- | A renderer context object.
--
-- For type `Renderer px`...
--
--  px is a pixel format. Typically use `VRGBA (UNorm Word8)` for 8bit format.
data Renderer px = Renderer {
    rendererRenderCore :: RenderCore,

    -- Renderer Properties
    rendererImageFormat :: Vk.Format,
    rendererImageLayout :: Vk.ImageLayout,

    -- Render Pass
    rendererRenderPass :: Vk.RenderPass,

    -- Stencil Pipeline
    rendererStencilPipeline :: Vk.Pipeline,

    -- Pattern Solid
    rendererPatternSolid :: Vk.Pipeline,
    rendererPatternLinear :: Vk.Pipeline,
    rendererPatternRadial :: Vk.Pipeline,
    rendererPatternImage :: Vk.Pipeline
}

newRenderer :: (MonadIO m, Format px) => RenderCore -> Vk.ImageLayout -> m (Renderer px)
newRenderer renderCore rendererImageLayout = newRendererOf renderCore rendererImageLayout undefined

newRendererOf :: (MonadIO m, Format px) => RenderCore -> Vk.ImageLayout -> px -> m (Renderer px)
newRendererOf rendererRenderCore rendererImageLayout px = do
    let RenderCore {..} = rendererRenderCore
        Environment {..} = renderCoreEnvironment
        rendererImageFormat = formatOf px

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

    let stencilPipelineCreateInfo = baseStencilPipelineInfo
            renderCoreStencilPipelineLayout
            rendererRenderPass
            renderCoreStencilShaderVert
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

    (_, pipelines) <- Vk.createGraphicsPipelines
        environmentDevice
        Vk.NULL_HANDLE
        (V.fromList [
            Vk.SomeStruct $ basePatternPipelineCreateInfo
                renderCorePatternLayout
                rendererRenderPass
                renderCorePatternShaderVert
                renderCorePatternSolidShaderFrag,
            Vk.SomeStruct $ basePatternPipelineCreateInfo
                renderCorePatternLayout
                rendererRenderPass
                renderCorePatternShaderVert
                renderCorePatternLinearShaderFrag,
            Vk.SomeStruct $ basePatternPipelineCreateInfo
                renderCorePatternLayout
                rendererRenderPass
                renderCorePatternShaderVert
                renderCorePatternRadialShaderFrag,
            Vk.SomeStruct $ basePatternPipelineCreateInfo
                renderCorePatternLayout
                rendererRenderPass
                renderCorePatternShaderVert
                renderCorePatternImageShaderFrag
        ] )
        Nothing
        -- TOOD: Move else to here!


    let (rendererPatternSolid, rendererPatternLinear, rendererPatternRadial, rendererPatternImage) =
            case V.toList pipelines of
                [a, b, c, d] -> (a, b, c, d)
                _ -> error "newRenderer: Got incorrect number of pipeline!"

    pure Renderer {..}


freeRenderer :: (MonadIO m) => Renderer px -> m ()
freeRenderer Renderer {..} = do
    let RenderCore {..} = rendererRenderCore 
        Environment {..} = renderCoreEnvironment
    Vk.destroyPipeline environmentDevice rendererPatternImage Nothing
    Vk.destroyPipeline environmentDevice rendererPatternRadial Nothing
    Vk.destroyPipeline environmentDevice rendererPatternLinear Nothing
    Vk.destroyPipeline environmentDevice rendererPatternSolid Nothing

    Vk.destroyPipeline environmentDevice rendererStencilPipeline Nothing

    Vk.destroyRenderPass environmentDevice rendererRenderPass Nothing



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