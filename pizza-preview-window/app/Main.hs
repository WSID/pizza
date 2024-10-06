{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad
import Control.Exception

import Data.Int
import Data.Word

import Data.Bits
import Data.Foldable
import Data.Traversable
import Data.IORef
import Data.Function

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

import System.Exit

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Data.Vector (Vector, (!))
import qualified Data.Vector as V

import qualified Graphics.UI.GLFW as GLFW

import qualified Vulkan as Vk
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Zero as Vk
import Vulkan.Exception (VulkanException (..))
import qualified Vulkan.Utils.ShaderQQ.GLSL.Shaderc as Vku


data DeviceSelection = DeviceSelection {
    devSelectionScore :: Int,
    devSelectionGraphicQueueFamilyIndex :: Int
}

data DeviceState = DeviceState {
    deviceStateInst :: Vk.Instance,
    deviceStatePDevice :: Vk.PhysicalDevice,
    deviceStateMemoryProps :: Vk.PhysicalDeviceMemoryProperties,
    deviceStateDev :: Vk.Device,
    deviceStateGQueueFI :: Word32,
    deviceStateGQueue :: Vk.Queue
}

data SurfaceState = SurfaceState {
    surfaceStateSurface :: Vk.SurfaceKHR,
    surfaceStateMinImage :: Word32,
    surfaceStateFormat :: Vk.SurfaceFormatKHR
}

data SwapchainState = SwapchainState {
    swapchainStateSize :: Vk.Extent2D,
    swapchainStateSwapchain :: Vk.SwapchainKHR
}

data PipelineState = PipelineState {
    pipelineStateVertexShader :: Vk.ShaderModule,
    pipelineStateFragmentShader :: Vk.ShaderModule,
    pipelineStateDescriptorSetLayout :: Vk.DescriptorSetLayout,
    pipelineStateLayout :: Vk.PipelineLayout,
    pipelineStateRenderPass :: Vk.RenderPass,
    pipelineStatePipeline :: Vk.Pipeline
}

data FrameState = FrameState {
    frameStateImageView :: Vk.ImageView,
    frameStateFramebuffer :: Vk.Framebuffer
}

data BufferState = BufferState {
    bufferStateMemory :: Vk.DeviceMemory,
    bufferStateBuffer :: Vk.Buffer,
    bufferStateSize :: Int
}

data DrawState = DrawState {
    drawStateCommandPool :: Vk.CommandPool,
    drawStateCommandBuffer :: Vk.CommandBuffer,
    drawStateDescriptorPool :: Vk.DescriptorPool,
    drawStateDescriptorSet :: Vk.DescriptorSet,
    drawStateVertexBuffer :: BufferState,
    drawStateIndexBuffer :: BufferState,
    drawStateUniformBuffer :: BufferState,
    drawStateFence :: Vk.Fence,
    drawStateSemImage :: Vk.Semaphore,
    drawStateSemRender :: Vk.Semaphore
}

deviceSelection :: Vk.Instance -> Vk.PhysicalDevice -> IO (Maybe DeviceSelection)
deviceSelection inst device = do
    -- Device
    _ <- Vk.getPhysicalDeviceProperties device -- Currently we don't check this anyway.

    qprops <- Vk.getPhysicalDeviceQueueFamilyProperties device
    let iqprops = V.indexed qprops

    graphicsQueueFamilies <- iqprops &
            V.filterM (\(i, prop) -> do
                let isGraphicQueue = Vk.queueFlags prop .&. Vk.QUEUE_GRAPHICS_BIT /= zeroBits
                hasPresentation <- GLFW.getPhysicalDevicePresentationSupport
                    (Vk.instanceHandle inst)
                    (Vk.physicalDeviceHandle device)
                    (fromIntegral i)
                pure (isGraphicQueue && hasPresentation)
            )

    pure $ if null graphicsQueueFamilies then Nothing
        else Just $ DeviceSelection {
            devSelectionScore = 1,
            devSelectionGraphicQueueFamilyIndex = fst $ V.head graphicsQueueFamilies
        }


createDevState :: IO DeviceState
createDevState = do
    let appInfo = (Vk.zero :: Vk.ApplicationInfo) {
        Vk.applicationName = Just $ BSC.pack "pizza-preview-window"
    }

    let instLayers = V.singleton $ BSC.pack "VK_LAYER_KHRONOS_validation"

    instExtensionsGLFW <- GLFW.getRequiredInstanceExtensions
    instExtensionsVK <- V.fromList <$> traverse BS.packCString instExtensionsGLFW

    let instCreateInfo = (Vk.zero :: Vk.InstanceCreateInfo '[]) {
        Vk.applicationInfo = Just appInfo,
        Vk.enabledLayerNames = instLayers,
        Vk.enabledExtensionNames = instExtensionsVK
    }
    deviceStateInst <- Vk.createInstance instCreateInfo Nothing
    putStrLn "Vulkan is initialized"

    -- Pick devices
    (_, pdevices) <- Vk.enumeratePhysicalDevices deviceStateInst
    pdevicesWithScore <- flip V.mapMaybeM pdevices $ \pdevice -> do
         selection <- deviceSelection deviceStateInst pdevice
         pure $ fmap (\s -> (s, pdevice)) selection

    when (null pdevicesWithScore) $ die "No suitable device!"

    let (selection, deviceStatePDevice) = V.maximumOn (devSelectionScore . fst) pdevicesWithScore

    deviceStateMemoryProps <- Vk.getPhysicalDeviceMemoryProperties deviceStatePDevice

    -- Create Device
    let devExtensions = V.singleton Vk.KHR_SWAPCHAIN_EXTENSION_NAME

    let deviceStateGQueueFI = fromIntegral $
            devSelectionGraphicQueueFamilyIndex selection

    let graphicsQueueCreateInfo = (Vk.zero :: Vk.DeviceQueueCreateInfo '[]) {
        Vk.queueFamilyIndex = deviceStateGQueueFI,
        Vk.queuePriorities = V.fromList [1.0]
    }

    let devCreateInfo = (Vk.zero :: Vk.DeviceCreateInfo '[]) {
        Vk.queueCreateInfos = V.fromList [Vk.SomeStruct graphicsQueueCreateInfo],
        Vk.enabledExtensionNames = devExtensions
    }
    deviceStateDev <- Vk.createDevice deviceStatePDevice devCreateInfo Nothing
    deviceStateGQueue <- Vk.getDeviceQueue deviceStateDev deviceStateGQueueFI 0

    pure DeviceState { .. }

destroyDevState :: DeviceState -> IO ()
destroyDevState DeviceState { .. } = do
    Vk.destroyDevice deviceStateDev Nothing
    Vk.destroyInstance deviceStateInst Nothing


createSurfaceState :: DeviceState -> GLFW.Window -> IO SurfaceState
createSurfaceState DeviceState {..} window = do
    -- Create Surface
    let instGlfw = castPtr $ Vk.instanceHandle deviceStateInst

    surfaceStateSurface <- alloca $ \surfacePtr -> do
        res <- GLFW.createWindowSurface instGlfw window nullPtr (castPtr surfacePtr) :: IO Int32
        when (Vk.Result res /= Vk.SUCCESS) $ die ("Cannot create surface from window: Code = " ++ show res)
        peek surfacePtr

    surfaceCapability <- Vk.getPhysicalDeviceSurfaceCapabilitiesKHR
        deviceStatePDevice
        surfaceStateSurface

    let Vk.SurfaceCapabilitiesKHR {
        Vk.minImageCount = surfaceStateMinImage
    } = surfaceCapability

    (_, sformats) <- Vk.getPhysicalDeviceSurfaceFormatsKHR
        deviceStatePDevice
        surfaceStateSurface

    let mformat = V.find (\Vk.SurfaceFormatKHR {..} -> format == Vk.FORMAT_B8G8R8A8_UNORM) sformats
    let surfaceStateFormat = case mformat of
            Just v -> v
            Nothing -> error "Cannot find format!"

    pure SurfaceState {..}

destroySurfaceState :: DeviceState -> SurfaceState -> IO ()
destroySurfaceState DeviceState {..} SurfaceState {..} =
    Vk.destroySurfaceKHR deviceStateInst surfaceStateSurface Nothing


createSwapchainState :: DeviceState -> SurfaceState -> Int -> Int -> IO SwapchainState
createSwapchainState DeviceState {..} SurfaceState {..} width height = do
    let swapchainStateSize = Vk.Extent2D {
        Vk.width = fromIntegral width,
        Vk.height = fromIntegral height
    }

    let Vk.SurfaceFormatKHR {
        Vk.format = imageFormat,
        Vk.colorSpace = imageColorSpace
    } = surfaceStateFormat

    let swapchainCreateInfo = (Vk.zero :: Vk.SwapchainCreateInfoKHR '[]) {
        Vk.surface = surfaceStateSurface,
        Vk.minImageCount = surfaceStateMinImage + 1,
        Vk.imageFormat = imageFormat,
        Vk.imageColorSpace = imageColorSpace,
        Vk.imageExtent = swapchainStateSize,
        Vk.imageArrayLayers = 1,
        Vk.imageUsage = Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
        Vk.imageSharingMode = Vk.SHARING_MODE_EXCLUSIVE,
        Vk.preTransform = Vk.SURFACE_TRANSFORM_IDENTITY_BIT_KHR,
        Vk.presentMode = Vk.PRESENT_MODE_FIFO_KHR,
        Vk.compositeAlpha = Vk.COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
        Vk.clipped = True
    }

    swapchainStateSwapchain <- Vk.createSwapchainKHR deviceStateDev swapchainCreateInfo Nothing

    pure $ SwapchainState {..}

destroySwapchainState :: DeviceState -> SwapchainState -> IO ()
destroySwapchainState DeviceState {..} SwapchainState {..} =
    Vk.destroySwapchainKHR deviceStateDev swapchainStateSwapchain Nothing

createPipelineState :: DeviceState -> SurfaceState -> IO PipelineState
createPipelineState DeviceState {..} SurfaceState {..} = do
    -- PipelineLayout

    let descriptorSetLayoutBindingSize = (Vk.zero :: Vk.DescriptorSetLayoutBinding) {
        Vk.binding = 0,
        Vk.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER,
        Vk.descriptorCount = 1,
        Vk.stageFlags = Vk.SHADER_STAGE_VERTEX_BIT
    }

    let descriptorSetLayoutCreateInfo = (Vk.zero :: Vk.DescriptorSetLayoutCreateInfo '[]) {
        Vk.bindings = V.singleton descriptorSetLayoutBindingSize
    }

    pipelineStateDescriptorSetLayout <- Vk.createDescriptorSetLayout
        deviceStateDev
        descriptorSetLayoutCreateInfo
        Nothing

    let pipelineLayoutCreateInfo = (Vk.zero :: Vk.PipelineLayoutCreateInfo) {
        Vk.setLayouts = V.singleton pipelineStateDescriptorSetLayout
    }

    pipelineStateLayout <- Vk.createPipelineLayout
        deviceStateDev
        pipelineLayoutCreateInfo
        Nothing

    let Vk.SurfaceFormatKHR {
        Vk.format = imageFormat,
        Vk.colorSpace = _
    } = surfaceStateFormat

    -- RenderPass
    let colorAttachment = (Vk.zero :: Vk.AttachmentDescription) {
        Vk.format = imageFormat,
        Vk.samples = Vk.SAMPLE_COUNT_1_BIT,
        Vk.loadOp = Vk.ATTACHMENT_LOAD_OP_CLEAR,
        Vk.storeOp = Vk.ATTACHMENT_STORE_OP_STORE,
        Vk.stencilLoadOp = Vk.ATTACHMENT_LOAD_OP_DONT_CARE,
        Vk.stencilStoreOp = Vk.ATTACHMENT_STORE_OP_DONT_CARE,
        Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED,
        Vk.finalLayout = Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
    }

    let subpass = (Vk.zero :: Vk.SubpassDescription) {
        Vk.pipelineBindPoint = Vk.PIPELINE_BIND_POINT_GRAPHICS,
        Vk.colorAttachments = V.singleton $ Vk.AttachmentReference {
            Vk.attachment = 0,
            Vk.layout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        }
    }

    let subpassDep = (Vk.zero :: Vk.SubpassDependency) {
        Vk.srcSubpass = Vk.SUBPASS_EXTERNAL,
        Vk.srcStageMask = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
        Vk.dstStageMask = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
        Vk.dstAccessMask = Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
    }

    let renderPassCreateInfo = (Vk.zero :: Vk.RenderPassCreateInfo '[]) {
        Vk.attachments = V.fromList [colorAttachment],
        Vk.subpasses = V.fromList [subpass],
        Vk.dependencies = V.fromList [subpassDep]
    }

    pipelineStateRenderPass <- Vk.createRenderPass
        deviceStateDev
        renderPassCreateInfo
        Nothing


    -- Pipeline
    --   Vertex Input
    let vertexInputState = (Vk.zero :: Vk.PipelineVertexInputStateCreateInfo '[]) {
        Vk.vertexBindingDescriptions = V.fromList [
            Vk.VertexInputBindingDescription {
                Vk.binding = 0,
                Vk.stride = 8,
                Vk.inputRate = Vk.VERTEX_INPUT_RATE_VERTEX
            }
        ],
        Vk.vertexAttributeDescriptions = V.fromList [
            Vk.VertexInputAttributeDescription {
                Vk.location = 0,
                Vk.binding = 0,
                Vk.format = Vk.FORMAT_R32G32_SFLOAT,
                Vk.offset = 0
            }
        ]
    }

    let inputAssemblyStateCreateInfo = (Vk.zero :: Vk.PipelineInputAssemblyStateCreateInfo) {
        Vk.topology = Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
        Vk.primitiveRestartEnable = False
    }

    --   Shader Stages
    let vertexShaderCreateInfo = (Vk.zero :: Vk.ShaderModuleCreateInfo '[]) {
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

    pipelineStateVertexShader <- Vk.createShaderModule
        deviceStateDev
        vertexShaderCreateInfo
        Nothing

    let vertexStage = (Vk.zero :: Vk.PipelineShaderStageCreateInfo '[]) {
        Vk.stage = Vk.SHADER_STAGE_VERTEX_BIT,
        Vk.name = BSC.pack "main",
        Vk.module' = pipelineStateVertexShader
    }

    let fragmentShaderCreateInfo = (Vk.zero :: Vk.ShaderModuleCreateInfo '[]) {
        Vk.code = [Vku.frag|
            #version 450

            layout (location = 0)
            out vec4 color;

            void main () {
                color = vec4(1.0f, 1.0f, 1.0f, 1.0f);
            }
        |]
    }

    pipelineStateFragmentShader <- Vk.createShaderModule
        deviceStateDev
        fragmentShaderCreateInfo
        Nothing

    let fragmentStage = (Vk.zero :: Vk.PipelineShaderStageCreateInfo '[]) {
        Vk.stage = Vk.SHADER_STAGE_FRAGMENT_BIT,
        Vk.module' = pipelineStateFragmentShader,
        Vk.name = BSC.pack "main"
    }

    let viewportState = (Vk.zero :: Vk.PipelineViewportStateCreateInfo '[]) {
        Vk.viewportCount = 1,
        Vk.scissorCount = 1
    }

    let rasterizationSatateCreateInfo = (Vk.zero :: Vk.PipelineRasterizationStateCreateInfo '[]) {
        Vk.polygonMode = Vk.POLYGON_MODE_FILL,
        Vk.cullMode = Vk.CULL_MODE_BACK_BIT,
        Vk.frontFace = Vk.FRONT_FACE_CLOCKWISE,
        Vk.lineWidth = 1,
        Vk.depthClampEnable = False,
        Vk.depthBiasEnable = False
    }

    let multisampleState = (Vk.zero :: Vk.PipelineMultisampleStateCreateInfo '[]) {
        Vk.rasterizationSamples = Vk.SAMPLE_COUNT_1_BIT
    }

    let colorBlendState = (Vk.zero :: Vk.PipelineColorBlendStateCreateInfo '[]) {
        Vk.logicOpEnable = False,
        Vk.attachmentCount = 1,
        Vk.attachments = V.fromList [
            Vk.zero {
                Vk.colorWriteMask = getIor $ foldMap Ior [
                    Vk.COLOR_COMPONENT_R_BIT,
                    Vk.COLOR_COMPONENT_G_BIT,
                    Vk.COLOR_COMPONENT_B_BIT,
                    Vk.COLOR_COMPONENT_A_BIT
                ],
                Vk.blendEnable = False
            }
        ]
    }

    let dynamicState = (Vk.zero :: Vk.PipelineDynamicStateCreateInfo) {
        Vk.dynamicStates = V.fromList [
            Vk.DYNAMIC_STATE_VIEWPORT,
            Vk.DYNAMIC_STATE_SCISSOR
        ]
    }

    let pipelineCreateInfo = (Vk.zero :: Vk.GraphicsPipelineCreateInfo '[]) {
        Vk.stageCount = 2,
        Vk.stages = V.fromList [
            Vk.SomeStruct vertexStage,
            Vk.SomeStruct fragmentStage
        ],
        Vk.vertexInputState = Just $ Vk.SomeStruct vertexInputState,
        Vk.inputAssemblyState = Just inputAssemblyStateCreateInfo,
        Vk.viewportState = Just $ Vk.SomeStruct viewportState,
        Vk.rasterizationState = Just $ Vk.SomeStruct rasterizationSatateCreateInfo,
        Vk.multisampleState = Just $ Vk.SomeStruct multisampleState,
        Vk.depthStencilState = Nothing,
        Vk.colorBlendState = Just $ Vk.SomeStruct colorBlendState,
        Vk.dynamicState = Just dynamicState,
        Vk.layout = pipelineStateLayout,
        Vk.renderPass = pipelineStateRenderPass,
        Vk.subpass = 0
    }

    (_, pipelines) <- Vk.createGraphicsPipelines
        deviceStateDev
        Vk.NULL_HANDLE
        (V.singleton $ Vk.SomeStruct pipelineCreateInfo)
        Nothing

    let [pipelineStatePipeline] = V.toList pipelines

    pure PipelineState {..}

destroyPipelineState :: DeviceState -> PipelineState -> IO ()
destroyPipelineState DeviceState {..} PipelineState {..} = do
    Vk.destroyPipeline deviceStateDev pipelineStatePipeline Nothing
    Vk.destroyShaderModule deviceStateDev pipelineStateFragmentShader Nothing
    Vk.destroyShaderModule deviceStateDev pipelineStateVertexShader Nothing
    Vk.destroyRenderPass deviceStateDev pipelineStateRenderPass Nothing
    Vk.destroyPipelineLayout deviceStateDev pipelineStateLayout Nothing
    Vk.destroyDescriptorSetLayout deviceStateDev pipelineStateDescriptorSetLayout Nothing


createFrameState :: DeviceState -> SurfaceState -> PipelineState -> SwapchainState -> Vk.Image -> IO FrameState
createFrameState DeviceState {..} SurfaceState {..} PipelineState {..} SwapchainState {..} image = do
    let Vk.SurfaceFormatKHR {
        Vk.format = imageFormat
    } = surfaceStateFormat

    let imageViewCreateInfo = (Vk.zero :: Vk.ImageViewCreateInfo '[]) {
        Vk.image = image,
        Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D,
        Vk.format = imageFormat,
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

    frameStateImageView <- Vk.createImageView
        deviceStateDev
        imageViewCreateInfo
        Nothing

    let Vk.Extent2D {
        Vk.width = imageWidth,
        Vk.height = imageHeight
    } = swapchainStateSize

    let framebufferCreateInfo = (Vk.zero :: Vk.FramebufferCreateInfo '[]) {
        Vk.renderPass = pipelineStateRenderPass,
        Vk.attachments = V.fromList [frameStateImageView],
        Vk.width = fromIntegral imageWidth,
        Vk.height = fromIntegral imageHeight,
        Vk.layers = 1
    }

    frameStateFramebuffer <- Vk.createFramebuffer
        deviceStateDev
        framebufferCreateInfo
        Nothing

    pure FrameState {..}

destroyFrameState :: DeviceState -> FrameState -> IO ()
destroyFrameState DeviceState {..} FrameState {..} = do
    Vk.destroyFramebuffer deviceStateDev frameStateFramebuffer Nothing
    Vk.destroyImageView deviceStateDev frameStateImageView Nothing


createBufferState :: DeviceState -> Int -> Vk.BufferUsageFlags -> IO BufferState
createBufferState DeviceState {..} size usage = do
    let sizew = fromIntegral size
    let bufferCreateInfo = (Vk.zero :: Vk.BufferCreateInfo '[]) {
        Vk.size = sizew,
        Vk.usage = usage,
        Vk.sharingMode = Vk.SHARING_MODE_EXCLUSIVE,
        Vk.queueFamilyIndices = V.singleton deviceStateGQueueFI
    }

    bufferStateBuffer <- Vk.createBuffer deviceStateDev bufferCreateInfo Nothing

    memReq <- Vk.getBufferMemoryRequirements deviceStateDev bufferStateBuffer
    let Vk.MemoryRequirements {
        Vk.memoryTypeBits = memoryTypeBits,
        Vk.size = minSize
    } = memReq

    let actualSize = max minSize sizew

    let Just (index, _) = Vk.memoryTypes deviceStateMemoryProps &
            V.indexed &
            V.filter ( \(i, props) -> testBit memoryTypeBits i) &
            V.find ( \(i, props) -> Vk.propertyFlags props .&. (Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT) /= zeroBits)

    let memoryAllocateInfo = (Vk.zero :: Vk.MemoryAllocateInfo '[]) {
        Vk.allocationSize = actualSize,
        Vk.memoryTypeIndex = fromIntegral index
    }

    bufferStateMemory <- Vk.allocateMemory deviceStateDev memoryAllocateInfo Nothing

    Vk.bindBufferMemory deviceStateDev bufferStateBuffer bufferStateMemory 0

    pure BufferState {bufferStateSize = size, ..}

destroyBufferState :: DeviceState -> BufferState -> IO ()
destroyBufferState DeviceState {..} BufferState {..} = do
    Vk.destroyBuffer deviceStateDev bufferStateBuffer Nothing
    Vk.freeMemory deviceStateDev bufferStateMemory Nothing

createDrawState :: DeviceState -> PipelineState -> IO DrawState
createDrawState DeviceState {..} PipelineState {..} = do
    -- Command pool
    let commandPoolCreateInfo = (Vk.zero :: Vk.CommandPoolCreateInfo) {
        Vk.queueFamilyIndex = deviceStateGQueueFI
    }

    drawStateCommandPool <- Vk.createCommandPool
        deviceStateDev
        commandPoolCreateInfo
        Nothing


    -- Command Buffer

    let commandBufferAllocationInfo = (Vk.zero :: Vk.CommandBufferAllocateInfo) {
        Vk.commandPool = drawStateCommandPool,
        Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY,
        Vk.commandBufferCount = 1
    }

    buffers <- Vk.allocateCommandBuffers
        deviceStateDev
        commandBufferAllocationInfo

    let drawStateCommandBuffer = V.head buffers


    -- Descriptor Pool

    let descriptorPoolSize = Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER 1

    let descriptorSetCreateInfo = (Vk.zero :: Vk.DescriptorPoolCreateInfo '[]) {
        Vk.maxSets = 1,
        Vk.poolSizes = V.singleton descriptorPoolSize
    }

    drawStateDescriptorPool <- Vk.createDescriptorPool
        deviceStateDev
        descriptorSetCreateInfo
        Nothing


    -- Descriptor Set

    let descriptorSetAllocInfo = (Vk.zero :: Vk.DescriptorSetAllocateInfo '[]) {
        Vk.descriptorPool = drawStateDescriptorPool,
        Vk.setLayouts = V.singleton pipelineStateDescriptorSetLayout
    }

    descriptorSets <- Vk.allocateDescriptorSets
        deviceStateDev
        descriptorSetAllocInfo

    drawStateDescriptorSet <- case V.toList descriptorSets of
            [v] -> pure v
            _ -> die "Descriptor Set create failed"

    -- Vertex Buffers and Index Buffers

    drawStateVertexBuffer <- createBufferState
        DeviceState {..}
        (6 * sizeOf (undefined :: Float))
        Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT

    vertexPtr <- Vk.mapMemory
        deviceStateDev
        (bufferStateMemory drawStateVertexBuffer)
        0 (fromIntegral $ bufferStateSize drawStateVertexBuffer)
        Vk.zero

    let vertexData = castPtr vertexPtr :: Ptr Float

    pokeElemOff vertexData 0 200
    pokeElemOff vertexData 1 100

    pokeElemOff vertexData 2 300
    pokeElemOff vertexData 3 250

    pokeElemOff vertexData 4 0
    pokeElemOff vertexData 5 0

    Vk.unmapMemory
        deviceStateDev
        (bufferStateMemory drawStateVertexBuffer)

    drawStateIndexBuffer <- createBufferState
        DeviceState {..}
        (3 * sizeOf (undefined :: Word32))
        Vk.BUFFER_USAGE_INDEX_BUFFER_BIT

    indicePtr <- Vk.mapMemory
        deviceStateDev
        (bufferStateMemory drawStateIndexBuffer)
        0 (fromIntegral $ bufferStateSize drawStateIndexBuffer)
        Vk.zero

    let indiceData = castPtr indicePtr :: Ptr Word32

    pokeElemOff indiceData 0 0
    pokeElemOff indiceData 1 1
    pokeElemOff indiceData 2 2

    Vk.unmapMemory
        deviceStateDev
        (bufferStateMemory drawStateIndexBuffer)

    -- Fences and Semaphores

    let fenceCreateInfo = (Vk.zero :: Vk.FenceCreateInfo '[]) {
        Vk.flags = Vk.FENCE_CREATE_SIGNALED_BIT
    }

    drawStateFence <- Vk.createFence deviceStateDev fenceCreateInfo Nothing

    let semCreateInfo = Vk.zero :: Vk.SemaphoreCreateInfo '[]

    drawStateSemImage <- Vk.createSemaphore deviceStateDev semCreateInfo Nothing
    drawStateSemRender <- Vk.createSemaphore deviceStateDev semCreateInfo Nothing


    -- Uniform Buffer

    drawStateUniformBuffer <- createBufferState
        DeviceState {..}
        (2 * sizeOf (undefined :: Float)) -- for vec2
        Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT

    uniformPtr <- Vk.mapMemory
        deviceStateDev
        (bufferStateMemory drawStateUniformBuffer)
        0 (fromIntegral $ bufferStateSize drawStateUniformBuffer)
        Vk.zero

    let uniformData = castPtr uniformPtr :: Ptr Float

    pokeElemOff uniformData 0 400
    pokeElemOff uniformData 1 400

    Vk.unmapMemory
        deviceStateDev
        (bufferStateMemory drawStateUniformBuffer)

    -- Setup descriptor set

    let writeDescriptorSet = (Vk.zero :: Vk.WriteDescriptorSet '[]) {
        Vk.dstSet = drawStateDescriptorSet,
        Vk.dstBinding = 0,
        Vk.dstArrayElement = 0,
        Vk.descriptorCount = 1,
        Vk.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER,
        Vk.bufferInfo = V.singleton (
            Vk.DescriptorBufferInfo
                (bufferStateBuffer drawStateUniformBuffer) 0 Vk.WHOLE_SIZE
        )
    }

    Vk.updateDescriptorSets
        deviceStateDev
        -- Write
        (V.singleton (Vk.SomeStruct writeDescriptorSet))
        -- Copy
        V.empty


    pure DrawState {..}

destroyDrawState :: DeviceState -> DrawState -> IO ()
destroyDrawState DeviceState {..} DrawState {..} = do
    Vk.destroySemaphore deviceStateDev drawStateSemRender Nothing
    Vk.destroySemaphore deviceStateDev drawStateSemImage Nothing
    Vk.destroyFence deviceStateDev drawStateFence Nothing
    destroyBufferState DeviceState {..} drawStateUniformBuffer
    destroyBufferState DeviceState {..} drawStateIndexBuffer
    destroyBufferState DeviceState {..} drawStateVertexBuffer
    Vk.destroyDescriptorPool deviceStateDev drawStateDescriptorPool Nothing
    Vk.destroyCommandPool deviceStateDev drawStateCommandPool Nothing

draw :: DeviceState -> PipelineState -> DrawState -> SwapchainState -> (Int -> FrameState) -> IO ()
draw DeviceState {..} PipelineState {..} DrawState {..} SwapchainState {..} getFrameState = do
    _ <- Vk.waitForFences deviceStateDev (V.singleton drawStateFence) True maxBound
    Vk.resetFences deviceStateDev (V.singleton drawStateFence)

    putStrLn "Frame Start!"

    (imgRes, index) <- Vk.acquireNextImageKHR
        deviceStateDev
        swapchainStateSwapchain
        maxBound -- timeout in nanosecs
        drawStateSemImage
        Vk.NULL_HANDLE -- fence

    when (imgRes == Vk.NOT_READY) $ putStrLn "> ! Swapchain: Image Not Ready!"
    when (imgRes == Vk.SUBOPTIMAL_KHR) $ putStrLn "> ! Swapchain: Suboptimal!"

    putStrLn $ "> Image Acquired: " ++ show index

    let FrameState {..} = getFrameState $ fromIntegral index

    Vk.resetCommandPool deviceStateDev drawStateCommandPool zeroBits

    let beginInfo = (Vk.zero :: Vk.CommandBufferBeginInfo '[]) {
        Vk.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
    }

    Vk.useCommandBuffer drawStateCommandBuffer beginInfo $ do
        let Vk.Extent2D {
            Vk.width = swapchainWidth,
            Vk.height = swapchainHeight
        } = swapchainStateSize

        let viewport = Vk.Viewport 0 0 (fromIntegral swapchainWidth) (fromIntegral swapchainHeight) 0 1
        let scissor = Vk.Rect2D (Vk.Offset2D 0 0) swapchainStateSize

        Vk.cmdSetViewport drawStateCommandBuffer 0 (V.singleton viewport)
        Vk.cmdSetScissor drawStateCommandBuffer 0 (V.singleton scissor)

        let renderPassBeginInfo = (Vk.zero :: Vk.RenderPassBeginInfo '[]) {
            Vk.renderPass = pipelineStateRenderPass,
            Vk.framebuffer = frameStateFramebuffer,
            Vk.renderArea = Vk.Rect2D {
                Vk.offset = Vk.Offset2D 0 0,
                Vk.extent = Vk.Extent2D 400 400
            },
            Vk.clearValues = V.fromList [
                Vk.Color $ Vk.Float32 0 0 0 1
            ]
        }
        Vk.cmdUseRenderPass
            drawStateCommandBuffer
            renderPassBeginInfo
            Vk.SUBPASS_CONTENTS_INLINE
            $ do
                Vk.cmdBindPipeline
                    drawStateCommandBuffer
                    Vk.PIPELINE_BIND_POINT_GRAPHICS
                    pipelineStatePipeline

                Vk.cmdBindIndexBuffer drawStateCommandBuffer
                    (bufferStateBuffer drawStateIndexBuffer)
                    0
                    Vk.INDEX_TYPE_UINT32

                Vk.cmdBindVertexBuffers drawStateCommandBuffer
                    0
                    (V.singleton $ bufferStateBuffer drawStateVertexBuffer)
                    (V.singleton 0)

                Vk.cmdBindDescriptorSets drawStateCommandBuffer
                    Vk.PIPELINE_BIND_POINT_GRAPHICS
                    pipelineStateLayout
                    0
                    (V.singleton drawStateDescriptorSet)
                    (V.empty)

                Vk.cmdDrawIndexed drawStateCommandBuffer 3 1 0 0 0

    let submitInfo = (Vk.zero :: Vk.SubmitInfo '[]) {
        Vk.waitSemaphores = V.singleton drawStateSemImage,
        Vk.waitDstStageMask = V.singleton Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
        Vk.commandBuffers = V.singleton (Vk.commandBufferHandle drawStateCommandBuffer),
        Vk.signalSemaphores = V.singleton drawStateSemRender
    }

    putStrLn "> Command Submitted"

    Vk.queueSubmit
        deviceStateGQueue
        (V.singleton $ Vk.SomeStruct submitInfo)
        drawStateFence

    catch
        (do
            let presentInfo = (Vk.zero :: Vk.PresentInfoKHR '[]) {
                Vk.waitSemaphores = V.singleton drawStateSemRender,
                Vk.swapchains = V.singleton swapchainStateSwapchain,
                Vk.imageIndices = V.singleton index
            }

            _ <- Vk.queuePresentKHR
                deviceStateGQueue
                presentInfo

            putStrLn "> Present Submitted"
        )
        (\(VulkanException _) -> do
            putStrLn "> Present Submitted (Something wrong!)"
            Vk.deviceWaitIdle deviceStateDev
        )

    pure ()


main :: IO ()
main = do
    initSucc <- GLFW.init

    unless initSucc $ die "GLFW initialization failed."

    vulkanSupported <- GLFW.vulkanSupported
    unless vulkanSupported $ die "Vulkan is not supported!"

    devState <- createDevState
    putStrLn "Device Initialized"

    GLFW.windowHint (GLFW.WindowHint'Resizable False)
    GLFW.windowHint (GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI)

    mwin <- GLFW.createWindow 400 400 "Pizza Preview" Nothing Nothing
    win <- case mwin of
        Just win -> pure win
        Nothing -> die "Window initialization failed."

    (width, height) <- GLFW.getWindowSize win

    surfaceState <- createSurfaceState devState win
    swapchainState <- createSwapchainState devState surfaceState width height
    pipelineState <- createPipelineState devState surfaceState
    drawState <- createDrawState devState pipelineState

    (_, images) <- Vk.getSwapchainImagesKHR
        (deviceStateDev devState)
        (swapchainStateSwapchain swapchainState)

    frameStates <- for images $ createFrameState devState surfaceState pipelineState swapchainState

    keepAlive <- newIORef True
    GLFW.setWindowCloseCallback win $ Just (\_ -> writeIORef keepAlive False)
    GLFW.showWindow win

    fix $ \recur -> do
        draw devState pipelineState drawState swapchainState (frameStates !)
        Vk.deviceWaitIdle (deviceStateDev devState)
        a <- readIORef keepAlive
        when a (GLFW.waitEvents >> recur)

    for_ frameStates $ destroyFrameState devState

    destroyDrawState devState drawState
    destroyPipelineState devState pipelineState
    destroySwapchainState devState swapchainState
    destroySurfaceState devState surfaceState

    GLFW.destroyWindow win

    destroyDevState devState

    GLFW.terminate



