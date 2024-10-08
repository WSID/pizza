{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Graphics.Pizza where

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

-- bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

-- vulkan
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import qualified Vulkan.CStruct.Extends as Vk

-- vulkan utils
import qualified Vulkan.Utils.ShaderQQ.GLSL.Shaderc as Vku

-- VulkanMemoryAllocator
import qualified VulkanMemoryAllocator as Vma


data Context = Context {
    -- Note: I may separate this as two.
    --     - Device
    --     - Pipeline

    -- Device and Allocator
    contextDevice :: Vk.Device,

    -- Common properties
    contextQueueFamilyIndexGraphics :: Word32,

    -- Render Pass
    contextRenderPass :: Vk.RenderPass,

    -- Shaders
    contextShaderVert :: Vk.ShaderModule,
    contextShaderFrag :: Vk.ShaderModule,

    -- Pipelines
    contextDescriptorSetLayout :: Vk.DescriptorSetLayout,
    contextPipelineLayout :: Vk.PipelineLayout,
    contextPipeline :: Vk.Pipeline,

    -- Pools
    contextDescriptorPool :: Vk.DescriptorPool,
    contextCommandPool :: Vk.CommandPool
}

-- | Rendering Operation
data Render = Render {
    renderAllocator :: Vma.Allocator,
    renderCommandBuffer :: Vk.CommandBuffer,
    renderDescriptorSet :: Vk.DescriptorSet,
    renderBufferVertexAlloc :: Vma.Allocation,
    renderBufferVertex :: Vk.Buffer,
    renderBufferIndexAlloc :: Vma.Allocation,
    renderBufferIndex :: Vk.Buffer,
    renderBufferUniformAlloc :: Vma.Allocation,
    renderBufferUniform :: Vk.Buffer
}

data RenderTarget = RenderTarget {
    renderTargetSize :: Vk.Extent2D,
    renderTargetImageView :: Vk.ImageView,
    renderTargetFramebuffer :: Vk.Framebuffer
}

newContext :: (MonadIO m) => Vk.Device -> Word32 -> Vk.Format -> Vk.ImageLayout -> m Context
newContext device contextQueueFamilyIndexGraphics format layout = do
    let contextDevice = device

    contextRenderPass <- Vk.createRenderPass
        device
        Vk.zero {   -- Vk.RenderPassCreateInfo
            Vk.attachments = V.singleton (Vk.zero :: Vk.AttachmentDescription) {
                Vk.format = format,
                Vk.samples = Vk.SAMPLE_COUNT_1_BIT,
                Vk.loadOp = Vk.ATTACHMENT_LOAD_OP_CLEAR,
                Vk.storeOp = Vk.ATTACHMENT_STORE_OP_STORE,
                Vk.stencilLoadOp = Vk.ATTACHMENT_LOAD_OP_DONT_CARE,
                Vk.stencilStoreOp = Vk.ATTACHMENT_STORE_OP_DONT_CARE,
                Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED,
                Vk.finalLayout = layout
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

    contextShaderVert <- Vk.createShaderModule
        device
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

    contextShaderFrag <- Vk.createShaderModule
        device
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

    contextDescriptorSetLayout <- Vk.createDescriptorSetLayout
        device
        Vk.zero {
            Vk.bindings = V.singleton Vk.zero {
                Vk.binding = 0,
                Vk.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                Vk.descriptorCount = 1,
                Vk.stageFlags = Vk.SHADER_STAGE_VERTEX_BIT
            }
        }
        Nothing

    contextPipelineLayout <- Vk.createPipelineLayout
        device
        Vk.zero {
            Vk.setLayouts = V.singleton contextDescriptorSetLayout
        }
        Nothing

    (_, pipelines) <- Vk.createGraphicsPipelines
        device
        Vk.NULL_HANDLE
        (V.singleton $ Vk.SomeStruct Vk.zero {
            Vk.next = (),
            Vk.stageCount = 2,
            Vk.stages = V.fromList [
                Vk.SomeStruct Vk.zero {
                    Vk.stage = Vk.SHADER_STAGE_VERTEX_BIT,
                    Vk.name = BSC.pack "main",
                    Vk.module' = contextShaderVert
                },
                Vk.SomeStruct Vk.zero {
                    Vk.stage = Vk.SHADER_STAGE_FRAGMENT_BIT,
                    Vk.name = BSC.pack "main",
                    Vk.module' = contextShaderFrag
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

            Vk.layout = contextPipelineLayout,
            Vk.renderPass = contextRenderPass,
            Vk.subpass = 0
        } )
        Nothing
        -- TOOD: Move else to here!

    let contextPipeline = V.head pipelines

    contextDescriptorPool <- Vk.createDescriptorPool
        device
        Vk.zero { -- Vk.DescriptorPoolCreateInfo
            Vk.flags = Vk.DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT,
            Vk.maxSets = 1,
            Vk.poolSizes = V.singleton $
                Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
        }
        Nothing

    contextCommandPool <- Vk.createCommandPool
        device
        Vk.zero { -- Vk.CommandPoolCreateInfo
            Vk.flags = Vk.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
            Vk.queueFamilyIndex = contextQueueFamilyIndexGraphics
        }
        Nothing

    pure Context {..}

freeContext :: (MonadIO m) => Context -> m ()
freeContext Context {..} = do
    Vk.destroyPipeline contextDevice contextPipeline Nothing
    Vk.destroyPipelineLayout contextDevice contextPipelineLayout Nothing
    Vk.destroyDescriptorSetLayout contextDevice contextDescriptorSetLayout Nothing
    Vk.destroyShaderModule contextDevice contextShaderFrag Nothing
    Vk.destroyShaderModule contextDevice contextShaderVert Nothing
    Vk.destroyRenderPass contextDevice contextRenderPass Nothing
    Vk.destroyCommandPool contextDevice contextCommandPool Nothing
    Vk.destroyDescriptorPool contextDevice contextDescriptorPool Nothing


newRender :: (MonadIO m) => Vma.Allocator -> Context -> m Render
newRender allocator Context {..} = do
    let renderAllocator = allocator

    commandBuffers <- Vk.allocateCommandBuffers
        contextDevice
        Vk.zero { -- Vk.CommandBufferAllocateInfo
            Vk.commandPool = contextCommandPool,
            Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY,
            Vk.commandBufferCount = 1
        }
    let renderCommandBuffer = V.head commandBuffers

    descriptorSets <- Vk.allocateDescriptorSets
        contextDevice
        Vk.zero { -- Vk.DescriptorSetAllocateInfo
            Vk.next = (),
            Vk.descriptorPool = contextDescriptorPool,
            Vk.setLayouts = V.singleton contextDescriptorSetLayout
        }
    let renderDescriptorSet = V.head descriptorSets

    (renderBufferVertex, renderBufferVertexAlloc, _) <- Vma.createBuffer
        allocator
        Vk.zero { -- Vk.BufferCreateInfo
            Vk.size = fromIntegral $ 4 * sizeOf (undefined :: V2 Float),
            Vk.usage = Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT,
            Vk.sharingMode = Vk.SHARING_MODE_EXCLUSIVE,
            Vk.queueFamilyIndices = V.singleton contextQueueFamilyIndexGraphics
        }
        Vk.zero { -- Vma.AllocationCreateInfo
            Vma.flags = Vma.ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT,
            Vma.usage = Vma.MEMORY_USAGE_AUTO
        }

    (renderBufferIndex, renderBufferIndexAlloc, _) <- Vma.createBuffer
        allocator
        Vk.zero { -- Vk.BufferCreateInfo
            Vk.size = fromIntegral $ 2 * sizeOf (undefined :: V3 Word32),
            Vk.usage = Vk.BUFFER_USAGE_INDEX_BUFFER_BIT,
            Vk.sharingMode = Vk.SHARING_MODE_EXCLUSIVE,
            Vk.queueFamilyIndices = V.singleton contextQueueFamilyIndexGraphics
        }
        Vk.zero { -- Vma.AllocationCreateInfo
            Vma.flags = Vma.ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT,
            Vma.usage = Vma.MEMORY_USAGE_AUTO
        }

    (renderBufferUniform, renderBufferUniformAlloc, _) <- Vma.createBuffer
        allocator
        Vk.zero { -- Vk.BufferCreateInfo
            Vk.size = fromIntegral $ sizeOf (undefined :: V2 Float),
            Vk.usage = Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT,
            Vk.sharingMode = Vk.SHARING_MODE_EXCLUSIVE,
            Vk.queueFamilyIndices = V.singleton contextQueueFamilyIndexGraphics
        }
        Vk.zero { -- Vma.AllocationCreateInfo
            Vma.flags = Vma.ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT,
            Vma.usage = Vma.MEMORY_USAGE_AUTO
        }

    -- Fill content
    -- TODO: Move each part to appropriate parts.

    Vk.updateDescriptorSets
        contextDevice
        -- writes
        (V.singleton $ Vk.SomeStruct Vk.zero {
            Vk.next = (),
            Vk.dstSet = renderDescriptorSet,
            Vk.dstBinding = 0,
            Vk.dstArrayElement = 0,
            Vk.descriptorCount = 1,
            Vk.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER,
            Vk.bufferInfo = V.singleton $ Vk.zero {
                Vk.buffer = renderBufferUniform,
                Vk.offset = 0,
                Vk.range = Vk.WHOLE_SIZE
            }
        } )
        -- copies
        V.empty

    vertexPtr <- Vma.mapMemory allocator renderBufferVertexAlloc
    liftIO $ pokeArray (castPtr vertexPtr) [V2 0 0, V2 200 0, V2 200 200, V2 0 200 :: V2 Float]
    Vma.unmapMemory allocator renderBufferVertexAlloc

    indexPtr <- Vma.mapMemory allocator renderBufferIndexAlloc
    liftIO $ pokeArray (castPtr indexPtr) [V3 0 1 2, V3 0 2 3 :: V3 Word32]
    Vma.unmapMemory allocator renderBufferIndexAlloc

    -- We'll initialize uniforms at later.

    pure Render {..}

freeRender :: (MonadIO m) => Context -> Render -> m ()
freeRender Context {..} Render {..} = do
    Vma.destroyBuffer renderAllocator renderBufferUniform renderBufferUniformAlloc
    Vma.destroyBuffer renderAllocator renderBufferIndex renderBufferIndexAlloc
    Vma.destroyBuffer renderAllocator renderBufferVertex renderBufferVertexAlloc
    Vk.freeDescriptorSets contextDevice contextDescriptorPool $ V.singleton renderDescriptorSet
    Vk.freeCommandBuffers contextDevice contextCommandPool $ V.singleton renderCommandBuffer

recordRender :: (MonadIO m) => Context -> Render -> RenderTarget -> m ()
recordRender Context {..} Render {..} RenderTarget {..} = do
    let cmdbuf = renderCommandBuffer
    let Vk.Extent2D {
        Vk.width = width,
        Vk.height = height
    } = renderTargetSize

    let renderArea = Vk.Rect2D {
        Vk.offset = Vk.Offset2D 0 0,
        Vk.extent = renderTargetSize
    }

    Vk.resetCommandBuffer cmdbuf zeroBits

    Vk.useCommandBuffer cmdbuf Vk.zero {
        Vk.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
    } $ do
        Vk.cmdSetViewport cmdbuf 0 $ V.singleton Vk.Viewport {
            Vk.x = 0,
            Vk.y = 0,
            Vk.width = fromIntegral width,
            Vk.height = fromIntegral height,
            Vk.minDepth = 0,
            Vk.maxDepth = 1
        }

        Vk.cmdSetScissor cmdbuf 0 $ V.singleton renderArea

        Vk.cmdUseRenderPass
            cmdbuf
            Vk.zero {
                Vk.renderPass = contextRenderPass,
                Vk.framebuffer = renderTargetFramebuffer,
                Vk.renderArea = renderArea,
                Vk.clearValues = V.singleton (Vk.Color $ Vk.Float32 0 0 0 1)
            }
            Vk.SUBPASS_CONTENTS_INLINE $ do

            Vk.cmdBindPipeline cmdbuf Vk.PIPELINE_BIND_POINT_GRAPHICS contextPipeline
            Vk.cmdBindIndexBuffer cmdbuf renderBufferIndex 0 Vk.INDEX_TYPE_UINT32

            Vk.cmdBindVertexBuffers cmdbuf 0
                (V.singleton renderBufferVertex)
                (V.singleton 0)

            Vk.cmdBindDescriptorSets cmdbuf
                Vk.PIPELINE_BIND_POINT_GRAPHICS
                contextPipelineLayout
                0
                (V.singleton renderDescriptorSet)
                (V.empty)

            Vk.cmdDrawIndexed renderCommandBuffer 6 1 0 0 0

        pure ()

newRenderTarget :: (MonadIO m) => Context -> Vk.Image -> Vk.Format -> Word -> Word -> m RenderTarget
newRenderTarget Context {..} image format width height = do
    let renderTargetSize = Vk.Extent2D (fromIntegral width) (fromIntegral height)
    renderTargetImageView <- Vk.createImageView
        contextDevice
        Vk.zero { -- Vk.ImageViewCreateInfo
            Vk.image = image,
            Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D,
            Vk.format = format,
            Vk.components = Vk.ComponentMapping
                Vk.COMPONENT_SWIZZLE_IDENTITY
                Vk.COMPONENT_SWIZZLE_IDENTITY
                Vk.COMPONENT_SWIZZLE_IDENTITY
                Vk.COMPONENT_SWIZZLE_IDENTITY,
            Vk.subresourceRange = Vk.ImageSubresourceRange {
                Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT ,
                Vk.baseMipLevel = 0,
                Vk.levelCount = 1,
                Vk.baseArrayLayer = 0,
                Vk.layerCount = 1
            }
        }
        Nothing

    renderTargetFramebuffer <- Vk.createFramebuffer
        contextDevice
        Vk.zero {
            Vk.renderPass = contextRenderPass,
            Vk.attachments = V.singleton renderTargetImageView,
            Vk.width = fromIntegral width,
            Vk.height = fromIntegral height,
            Vk.layers = 1
        }
        Nothing

    pure RenderTarget {..}

freeRenderTarget :: (MonadIO m) => Context -> RenderTarget -> m ()
freeRenderTarget Context {..} renderTarget = do
    Vk.destroyFramebuffer contextDevice (renderTargetFramebuffer renderTarget) Nothing
    Vk.destroyImageView contextDevice (renderTargetImageView renderTarget) Nothing


render :: (MonadIO m) => Vk.Queue -> Maybe Vk.Semaphore -> Vector Vk.Semaphore -> Vk.Fence -> Context -> Render -> RenderTarget -> m ()
render queue wait signal fence Context {..} Render {..} RenderTarget {..} = do
    recordRender Context {..} Render {..} RenderTarget {..}

    uniformPtr <- Vma.mapMemory renderAllocator renderBufferUniformAlloc

    let Vk.Extent2D {
        Vk.width = rtWidth,
        Vk.height = rtHeight
    } = renderTargetSize

    liftIO $ poke (castPtr uniformPtr) (V2 (fromIntegral rtWidth) (fromIntegral rtHeight) :: V2 Float)
    Vma.unmapMemory renderAllocator renderBufferUniformAlloc

    case wait of
        Just w -> Vk.queueSubmit queue
            (V.singleton $ Vk.SomeStruct Vk.zero {
                Vk.waitSemaphores = V.singleton w,
                Vk.waitDstStageMask = V.singleton Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
                Vk.commandBuffers = V.singleton $ Vk.commandBufferHandle renderCommandBuffer,
                Vk.signalSemaphores = signal
            } )
            fence
        Nothing -> Vk.queueSubmit queue
            (V.singleton $ Vk.SomeStruct Vk.zero {
                Vk.waitSemaphores = V.empty,
                Vk.waitDstStageMask = V.empty,
                Vk.commandBuffers = V.singleton $ Vk.commandBufferHandle renderCommandBuffer,
                Vk.signalSemaphores = signal
            } )
            fence
