{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE DuplicateRecordFields #-}

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

import Data.Vector ((!))
import qualified Data.Vector as V

import qualified Graphics.UI.GLFW as GLFW

import qualified Vulkan as Vk
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Dynamic as Vk
import qualified Vulkan.Zero as Vk
import Vulkan.Exception (VulkanException (..))

import qualified VulkanMemoryAllocator as Vma

import Graphics.Pizza

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

data BufferState = BufferState {
    bufferStateMemory :: Vk.DeviceMemory,
    bufferStateBuffer :: Vk.Buffer,
    bufferStateSize :: Int
}

data DrawState = DrawState {
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


createRenderTarget ::  SurfaceState -> Context -> SwapchainState -> Vk.Image -> IO RenderTarget
createRenderTarget SurfaceState {..} Context {..} SwapchainState {..} image = do
    let Vk.SurfaceFormatKHR {
        Vk.format = imageFormat
    } = surfaceStateFormat

    let Vk.Extent2D {
        Vk.width = imageWidth,
        Vk.height = imageHeight
    } = swapchainStateSize

    newRenderTarget Context {..} image imageFormat (fromIntegral imageWidth) (fromIntegral imageHeight)


createDrawState :: DeviceState -> IO DrawState
createDrawState DeviceState {..} = do
    -- Fences and Semaphores

    let fenceCreateInfo = (Vk.zero :: Vk.FenceCreateInfo '[]) {
        Vk.flags = Vk.FENCE_CREATE_SIGNALED_BIT
    }

    drawStateFence <- Vk.createFence deviceStateDev fenceCreateInfo Nothing

    let semCreateInfo = Vk.zero :: Vk.SemaphoreCreateInfo '[]

    drawStateSemImage <- Vk.createSemaphore deviceStateDev semCreateInfo Nothing
    drawStateSemRender <- Vk.createSemaphore deviceStateDev semCreateInfo Nothing

    pure DrawState {..}

destroyDrawState :: DeviceState -> DrawState -> IO ()
destroyDrawState DeviceState {..} DrawState {..} = do
    Vk.destroySemaphore deviceStateDev drawStateSemRender Nothing
    Vk.destroySemaphore deviceStateDev drawStateSemImage Nothing
    Vk.destroyFence deviceStateDev drawStateFence Nothing

draw :: DeviceState -> Context -> Render -> DrawState -> SwapchainState -> (Int -> RenderTarget) -> IO ()
draw DeviceState {..} Context {..} Render {..} DrawState {..} SwapchainState {..} getRT = do
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

    let RenderTarget {..} = getRT $ fromIntegral index
    render
        deviceStateGQueue
        drawStateSemImage
        (V.singleton drawStateSemRender)
        drawStateFence
        Context {..}
        Render {..}
        RenderTarget {..}

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

    let Vk.Instance {
        Vk.instanceCmds = instFuncs
    } = deviceStateInst devState

    let Vk.Device {
        Vk.deviceCmds = devFuncs
    } = deviceStateDev devState

    allocator <- Vma.createAllocator Vk.zero {
        Vma.physicalDevice = Vk.physicalDeviceHandle $ deviceStatePDevice devState,
        Vma.device = Vk.deviceHandle $ deviceStateDev devState,
        Vma.instance' = Vk.instanceHandle $ deviceStateInst devState,
        Vma.vulkanFunctions = Just Vk.zero {
            Vma.vkGetInstanceProcAddr = castFunPtr $ Vk.pVkGetInstanceProcAddr instFuncs,
            Vma.vkGetDeviceProcAddr = castFunPtr $ Vk.pVkGetDeviceProcAddr devFuncs
        }
    }

    putStrLn "VulkanMemoryAllocator Initialized"

    GLFW.windowHint (GLFW.WindowHint'Resizable False)
    GLFW.windowHint (GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI)

    mwin <- GLFW.createWindow 400 400 "Pizza Preview" Nothing Nothing
    win <- case mwin of
        Just win -> pure win
        Nothing -> die "Window initialization failed."

    (width, height) <- GLFW.getWindowSize win

    surfaceState <- createSurfaceState devState win
    swapchainState <- createSwapchainState devState surfaceState width height

    let Vk.SurfaceFormatKHR {
        Vk.format = imageFormat
    } = surfaceStateFormat surfaceState

    context <- newContext (deviceStateDev devState) (deviceStateGQueueFI devState) imageFormat
    renderState <- newRender allocator context
    drawState <- createDrawState devState

    (_, images) <- Vk.getSwapchainImagesKHR
        (deviceStateDev devState)
        (swapchainStateSwapchain swapchainState)

    rts <- for images $ createRenderTarget surfaceState context swapchainState

    keepAlive <- newIORef True
    GLFW.setWindowCloseCallback win $ Just (\_ -> writeIORef keepAlive False)
    GLFW.showWindow win

    fix $ \recur -> do
        draw devState context renderState drawState swapchainState (rts !)
        Vk.deviceWaitIdle (deviceStateDev devState)
        a <- readIORef keepAlive
        when a (GLFW.waitEvents >> recur)

    for_ rts $ freeRenderTarget context

    destroyDrawState devState drawState
    freeRender context renderState
    freeContext context
    destroySwapchainState devState swapchainState
    destroySurfaceState devState surfaceState

    GLFW.destroyWindow win

    Vma.destroyAllocator allocator

    destroyDevState devState

    GLFW.terminate

