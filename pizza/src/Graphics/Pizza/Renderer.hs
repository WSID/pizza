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
import Data.Vector (Vector)

-- bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

-- vulkan
import qualified Vulkan as Vk
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Dynamic as Vk
import qualified Vulkan.Version as Vk
import qualified Vulkan.Zero as Vk

-- vulkan utils
import qualified Vulkan.Utils.ShaderQQ.GLSL.Shaderc as Vku

-- VulkanMemoryAllocator
import qualified VulkanMemoryAllocator as Vma


data Environment = Environment {
    environmentInst :: Vk.Instance,
    environmentPhysDevice :: Vk.PhysicalDevice,
    environmentDevice :: Vk.Device,
    environmentGraphicsQFI :: Word32,
    environmentGraphicsQueue :: Vk.Queue,
    environmentAllocator :: Vma.Allocator
}


data Renderer = Renderer {
    -- Renderer Properties
    rendererImageFormat :: Vk.Format,
    rendererImageLayout :: Vk.ImageLayout,

    -- Render Pass
    rendererRenderPass :: Vk.RenderPass,

    -- Shaders
    rendererShaderVert :: Vk.ShaderModule,
    rendererShaderFrag :: Vk.ShaderModule,

    -- Pipelines
    rendererDescriptorSetLayout :: Vk.DescriptorSetLayout,
    rendererPipelineLayout :: Vk.PipelineLayout,
    rendererPipeline :: Vk.Pipeline,

    -- Pools
    rendererDescriptorPool :: Vk.DescriptorPool,
    rendererCommandPool :: Vk.CommandPool
}

-- | Create a basic environment for vector graphics.
newBasicEnvironment :: MonadIO m => m Environment
newBasicEnvironment = do
    environmentInst <- Vk.createInstance Vk.zero {
        Vk.applicationInfo = Just Vk.zero {
            Vk.engineName = Just $ BSC.pack "pizza",
            Vk.engineVersion = Vk.MAKE_API_VERSION 0 0 0,
            Vk.apiVersion = Vk.API_VERSION_1_0
        }
    } Nothing

    (_, physDevices) <- Vk.enumeratePhysicalDevices environmentInst

    physDeviceGraphicsQFIs <- for physDevices $ \physDevice -> do
        qprops <- Vk.getPhysicalDeviceQueueFamilyProperties physDevice

        let queueCriteria Vk.QueueFamilyProperties {..} =
                queueFlags .&. Vk.QUEUE_GRAPHICS_BIT /= zeroBits

        pure $ fromIntegral <$> V.findIndex queueCriteria qprops

    let (environmentPhysDevice, environmentGraphicsQFI) = V.head
            $ V.mapMaybe (\(a, b) -> (,) <$> Just a <*> b)
            $ V.zip physDevices physDeviceGraphicsQFIs

    environmentDevice <- Vk.createDevice
        environmentPhysDevice
        Vk.zero {
            Vk.queueCreateInfos = V.singleton $ Vk.SomeStruct Vk.zero {
                Vk.queueFamilyIndex = environmentGraphicsQFI,
                Vk.queuePriorities = V.singleton 1.0
            }
        }
        Nothing

    environmentGraphicsQueue <- Vk.getDeviceQueue
        environmentDevice
        environmentGraphicsQFI
        0


    -- Allocator
    let Vk.Instance {
        Vk.instanceCmds = instFuncs
    } = environmentInst

    let Vk.Device {
        Vk.deviceCmds = devFuncs
    } = environmentDevice

    environmentAllocator <- Vma.createAllocator
        -- Vma.AllocatorCreateInfo
        Vk.zero {
            Vma.physicalDevice = Vk.physicalDeviceHandle environmentPhysDevice,
            Vma.device = Vk.deviceHandle environmentDevice,
            Vma.instance' = Vk.instanceHandle environmentInst,
            Vma.vulkanFunctions = Just Vk.zero {
                Vma.vkGetInstanceProcAddr = castFunPtr $ Vk.pVkGetInstanceProcAddr instFuncs,
                Vma.vkGetDeviceProcAddr = castFunPtr $ Vk.pVkGetDeviceProcAddr devFuncs
            }
        }

    pure Environment {..}

-- | Destroy environment.
freeEnvironment :: MonadIO m => Environment -> m ()
freeEnvironment Environment {..} = do
    Vma.destroyAllocator environmentAllocator
    Vk.destroyDevice environmentDevice Nothing
    Vk.destroyInstance environmentInst Nothing


newRenderer :: (MonadIO m) => Environment -> Vk.Format -> Vk.ImageLayout -> m Renderer
newRenderer Environment {..} rendererImageFormat rendererImageLayout = do
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

    rendererShaderVert <- Vk.createShaderModule
        environmentDevice
        Vk.zero { -- Vk.ShaderModuleCreateInfo
            Vk.code = [Vku.vert|
                #version 450

                layout (location = 0)
                in vec2 pos;

                layout (binding = 0) uniform Screen {
                    vec2 size;
                };

                void main () {
                    // Map (0, 0) ~ screenSize, to (-1, -1) ~ (+1, +1)
                    vec2 normPos = (pos / size * 2) - 1;

                    gl_Position = vec4(normPos, 0.0, 1.0);
                }
            |]
        }
        Nothing

    rendererShaderFrag <- Vk.createShaderModule
        environmentDevice
        Vk.zero { -- Vk.ShaderModuleCreateInfo
            Vk.code = [Vku.frag|
                #version 450

                layout (location = 0)
                out vec4 color;

                void main () {
                    color = vec4(1.0f, 1.0f, 1.0f, 1.0f);
                }
            |]
        }
        Nothing

    rendererDescriptorSetLayout <- Vk.createDescriptorSetLayout
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

    rendererPipelineLayout <- Vk.createPipelineLayout
        environmentDevice
        Vk.PipelineLayoutCreateInfo {
            Vk.flags = zeroBits,
            Vk.setLayouts = V.singleton rendererDescriptorSetLayout,
            Vk.pushConstantRanges = V.empty
        }
        Nothing

    (_, pipelines) <- Vk.createGraphicsPipelines
        environmentDevice
        Vk.NULL_HANDLE
        (V.singleton $ Vk.SomeStruct Vk.zero {
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
                    Vk.module' = rendererShaderFrag
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

            Vk.layout = rendererPipelineLayout,
            Vk.renderPass = rendererRenderPass,
            Vk.subpass = 0
        } )
        Nothing
        -- TOOD: Move else to here!

    let rendererPipeline = V.head pipelines

    rendererDescriptorPool <- Vk.createDescriptorPool
        environmentDevice
        Vk.zero { -- Vk.DescriptorPoolCreateInfo
            Vk.flags = Vk.DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT,
            Vk.maxSets = 1,
            Vk.poolSizes = V.singleton $
                Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
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


freeRenderer :: (MonadIO m) => Environment -> Renderer -> m ()
freeRenderer Environment {..} Renderer {..} = do
    Vk.destroyPipeline environmentDevice rendererPipeline Nothing
    Vk.destroyPipelineLayout environmentDevice rendererPipelineLayout Nothing
    Vk.destroyDescriptorSetLayout environmentDevice rendererDescriptorSetLayout Nothing
    Vk.destroyShaderModule environmentDevice rendererShaderFrag Nothing
    Vk.destroyShaderModule environmentDevice rendererShaderVert Nothing
    Vk.destroyRenderPass environmentDevice rendererRenderPass Nothing
    Vk.destroyCommandPool environmentDevice rendererCommandPool Nothing
    Vk.destroyDescriptorPool environmentDevice rendererDescriptorPool Nothing
