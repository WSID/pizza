{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Control.Monad
import Control.Exception

import Data.Int
import Data.Word

import Data.Bits
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
import qualified Vulkan.Zero as Vk
import Vulkan.Exception (VulkanException (..))
import qualified Vulkan.Dynamic as Vk

-- VulkanMemoryAllocator
import qualified VulkanMemoryAllocator as Vma

import qualified Graphics.Pizza as Pz
import qualified Graphics.Pizza.Preparation as Pz
import qualified Graphics.Pizza.Renderer as Pz
import qualified Graphics.Pizza.RenderTarget as Pz

data DeviceSelection = DeviceSelection {
    devSelectionScore :: Int,
    devSelectionGraphicQueueFamilyIndex :: Int
}

data SurfaceState = SurfaceState {
    surfaceStateSurface :: Vk.SurfaceKHR,
    surfaceStateMinImage :: Word32,
    surfaceStateFormat :: Vk.SurfaceFormatKHR
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

createEnvironment :: IO Pz.Environment
createEnvironment = do
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
    environmentInst <- Vk.createInstance instCreateInfo Nothing
    putStrLn "Vulkan is initialized"

    -- Pick devices
    (_, pdevices) <- Vk.enumeratePhysicalDevices environmentInst
    pdevicesWithScore <- flip V.mapMaybeM pdevices $ \pdevice -> do
         selection <- deviceSelection environmentInst pdevice
         pure $ fmap (\s -> (s, pdevice)) selection

    when (null pdevicesWithScore) $ die "No suitable device!"

    let (selection, environmentPhysDevice) = V.maximumOn (devSelectionScore . fst) pdevicesWithScore

    -- Create Device
    let devExtensions = V.singleton Vk.KHR_SWAPCHAIN_EXTENSION_NAME

    let environmentGraphicsQFI = fromIntegral $
            devSelectionGraphicQueueFamilyIndex selection

    let graphicsQueueCreateInfo = (Vk.zero :: Vk.DeviceQueueCreateInfo '[]) {
        Vk.queueFamilyIndex = environmentGraphicsQFI,
        Vk.queuePriorities = V.fromList [1.0]
    }

    let devCreateInfo = (Vk.zero :: Vk.DeviceCreateInfo '[]) {
        Vk.queueCreateInfos = V.fromList [Vk.SomeStruct graphicsQueueCreateInfo],
        Vk.enabledExtensionNames = devExtensions
    }
    environmentDevice <- Vk.createDevice environmentPhysDevice devCreateInfo Nothing
    environmentGraphicsQueue <- Vk.getDeviceQueue environmentDevice environmentGraphicsQFI 0

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


    pure Pz.Environment { .. }


createSurfaceState :: Pz.Environment -> GLFW.Window -> IO SurfaceState
createSurfaceState Pz.Environment {..} window = do
    -- Create Surface
    let instGlfw = castPtr $ Vk.instanceHandle environmentInst

    surfaceStateSurface <- alloca $ \surfacePtr -> do
        res <- GLFW.createWindowSurface instGlfw window nullPtr (castPtr surfacePtr) :: IO Int32
        when (Vk.Result res /= Vk.SUCCESS) $ die ("Cannot create surface from window: Code = " ++ show res)
        peek surfacePtr

    surfaceCapability <- Vk.getPhysicalDeviceSurfaceCapabilitiesKHR
        environmentPhysDevice
        surfaceStateSurface

    let Vk.SurfaceCapabilitiesKHR {
        Vk.minImageCount = surfaceStateMinImage
    } = surfaceCapability

    (_, sformats) <- Vk.getPhysicalDeviceSurfaceFormatsKHR
        environmentPhysDevice
        surfaceStateSurface

    let mformat = V.find (\Vk.SurfaceFormatKHR {..} -> format == Vk.FORMAT_B8G8R8A8_UNORM) sformats
    let surfaceStateFormat = case mformat of
            Just v -> v
            Nothing -> error "Cannot find format!"

    pure SurfaceState {..}


destroySurfaceState :: Pz.Environment -> SurfaceState -> IO ()
destroySurfaceState Pz.Environment {..} SurfaceState {..} =
    Vk.destroySurfaceKHR environmentInst surfaceStateSurface Nothing


createSwapchain :: Pz.Environment -> SurfaceState -> Int -> Int -> IO Vk.SwapchainKHR
createSwapchain Pz.Environment {..} SurfaceState {..} width height = do
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

    Vk.createSwapchainKHR environmentDevice swapchainCreateInfo Nothing


createDrawState :: Pz.Environment -> IO DrawState
createDrawState Pz.Environment {..} = do
    -- Fences and Semaphores

    let fenceCreateInfo = (Vk.zero :: Vk.FenceCreateInfo '[]) {
        Vk.flags = Vk.FENCE_CREATE_SIGNALED_BIT
    }

    drawStateFence <- Vk.createFence environmentDevice fenceCreateInfo Nothing

    let semCreateInfo = Vk.zero :: Vk.SemaphoreCreateInfo '[]

    drawStateSemImage <- Vk.createSemaphore environmentDevice semCreateInfo Nothing
    drawStateSemRender <- Vk.createSemaphore environmentDevice semCreateInfo Nothing

    pure DrawState {..}

destroyDrawState :: Pz.Environment -> DrawState -> IO ()
destroyDrawState Pz.Environment {..} DrawState {..} = do
    Vk.destroySemaphore environmentDevice drawStateSemRender Nothing
    Vk.destroySemaphore environmentDevice drawStateSemImage Nothing
    Vk.destroyFence environmentDevice drawStateFence Nothing

draw :: Pz.Environment -> Pz.Renderer -> Pz.Preparation -> DrawState -> Int -> Int -> Pz.SwapchainRenderTarget -> IO ()
draw Pz.Environment {..} Pz.Renderer {..} Pz.Preparation {..} DrawState {..} width height Pz.SwapchainRenderTarget {..} = do
    _ <- Vk.waitForFences environmentDevice (V.singleton drawStateFence) True maxBound
    Vk.resetFences environmentDevice (V.singleton drawStateFence)

    putStrLn "Frame Start!"

    (imgRes, index) <- Vk.acquireNextImageKHR
        environmentDevice
        renderTargetSwapchain
        maxBound -- timeout in nanosecs
        drawStateSemImage
        Vk.NULL_HANDLE -- fence

    when (imgRes == Vk.NOT_READY) $ putStrLn "> ! Swapchain: Image Not Ready!"
    when (imgRes == Vk.SUBOPTIMAL_KHR) $ putStrLn "> ! Swapchain: Suboptimal!"

    putStrLn $ "> Image Acquired: " ++ show index

    let Pz.BaseRenderTarget {..} = renderTargetBase ! fromIntegral index
    Pz.render
        Pz.Environment {..}
        (Just drawStateSemImage)
        (V.singleton drawStateSemRender)
        drawStateFence
        Pz.Renderer {..}
        Pz.Preparation {..}
        width height
        Pz.BaseRenderTarget {..}

    catch
        (do
            let presentInfo = (Vk.zero :: Vk.PresentInfoKHR '[]) {
                Vk.waitSemaphores = V.singleton drawStateSemRender,
                Vk.swapchains = V.singleton renderTargetSwapchain,
                Vk.imageIndices = V.singleton index
            }

            _ <- Vk.queuePresentKHR
                environmentGraphicsQueue
                presentInfo

            putStrLn "> Present Submitted"
        )
        (\(VulkanException _) -> do
            putStrLn "> Present Submitted (Something wrong!)"
            Vk.deviceWaitIdle environmentDevice
        )

    pure ()


main :: IO ()
main = do
    initSucc <- GLFW.init

    unless initSucc $ die "GLFW initialization failed."

    vulkanSupported <- GLFW.vulkanSupported
    unless vulkanSupported $ die "Vulkan is not supported!"

    environment <- createEnvironment
    putStrLn "Device Initialized"

    GLFW.windowHint (GLFW.WindowHint'Resizable False)
    GLFW.windowHint (GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI)

    mwin <- GLFW.createWindow 400 400 "Pizza Preview" Nothing Nothing
    win <- case mwin of
        Just win -> pure win
        Nothing -> die "Window initialization failed."

    (width, height) <- GLFW.getWindowSize win

    surfaceState <- createSurfaceState environment win
    swapchain <- createSwapchain environment surfaceState width height

    let Vk.SurfaceFormatKHR {
        Vk.format = imageFormat
    } = surfaceStateFormat surfaceState

    renderer <- Pz.newRenderer environment imageFormat Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
    preparation <- Pz.newPreparation environment renderer
    drawState <- createDrawState environment
    renderTarget <- Pz.newSwapchainRenderTarget environment renderer swapchain width height imageFormat

    keepAlive <- newIORef True
    GLFW.setWindowCloseCallback win $ Just (\_ -> writeIORef keepAlive False)
    GLFW.showWindow win

    fix $ \recur -> do
        draw environment renderer preparation drawState width height renderTarget
        Vk.deviceWaitIdle (Pz.environmentDevice environment)
        a <- readIORef keepAlive
        when a (GLFW.waitEvents >> recur)

    Pz.freeSwapchainRenderTarget environment renderTarget
    destroyDrawState environment drawState
    Pz.freePreparation environment renderer preparation
    Pz.freeRenderer environment renderer
    Vk.destroySwapchainKHR (Pz.environmentDevice environment) swapchain Nothing
    destroySurfaceState environment surfaceState

    GLFW.destroyWindow win

    Pz.freeEnvironment environment

    GLFW.terminate


