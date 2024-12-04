{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Control.Monad

import Data.Int
import Data.Word

import Data.Bits
import Data.IORef
import Data.Function

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

import System.Exit

import Numeric

import Linear

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import qualified Data.Vector as V

import qualified Graphics.UI.GLFW as GLFW

import qualified Vulkan as Vk
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Zero as Vk
import qualified Vulkan.Dynamic as Vk

-- VulkanMemoryAllocator
import qualified VulkanMemoryAllocator as Vma

import qualified Graphics.Pizza as Pz

-- time
import Data.Time.Clock

data DeviceSelection = DeviceSelection {
    devSelectionScore :: Int,
    devSelectionGraphicQueueFamilyIndex :: Int
}

data SurfaceState = SurfaceState {
    surfaceStateSurface :: Vk.SurfaceKHR,
    surfaceStateMinImage :: Word32,
    surfaceStateFormat :: Vk.SurfaceFormatKHR
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

makeGraphic :: Float -> Pz.Graphics
makeGraphic time = Pz.Graphics
    [
        Pz.Path
            [
                Pz.bezier
                    (V2 0 200)
                    [
                        V2 0 (0 - animValue1),
                        V2 400 (400 + animValue1)
                    ]
                    (V2 400 200),
                Pz.arc
                    (V2 (300 + animValue2) 200)
                    (100 - animValue2)
                    0
                    (negate pi),
                Pz.arc
                    (V2 (100 + animValue2) 200)
                    (100 + animValue2)
                    0
                    pi
            ],
        Pz.Path
            [
                Pz.arc
                    (V2 200 200)
                    (100 + animValue2)
                    0
                    (2 * pi)
            ]
    ]
    (Pz.PatternRadial
        (V2 (200 + 400 * cos theta) (400 + 400 * sin theta))
        400
        (V4 1 1 0 1)
        (V4 0 1 1 1)
    )
  where
    theta = time * 2
    animValue1 = 400 * cos theta
    animValue2 = 100 * sin theta

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
    renderTarget <- Pz.newSwapchainRenderTarget renderer swapchain width height imageFormat
    renderState <- Pz.newRenderStateSwapchain renderer

    keepAlive <- newIORef True
    GLFW.setWindowCloseCallback win $ Just (\_ -> writeIORef keepAlive False)
    GLFW.showWindow win

    timeStart <- getCurrentTime

    let loop recur timePrevFrame = do
            timeFrameStart <- getCurrentTime
            let timeDiff = realToFrac $ diffUTCTime timeFrameStart timeStart
            let graphics = makeGraphic timeDiff

            (_, presentWait) <-Pz.renderRenderStateTargetSwapchain renderer renderState graphics renderTarget

            timeFrameDone <- getCurrentTime

            presentWait

            let frameInterval = realToFrac $ diffUTCTime timeFrameStart timePrevFrame :: Float
            let renderInterval = realToFrac $ diffUTCTime timeFrameDone timeFrameStart :: Float

            putStrLn $ "FRAME: " ++ showFFloat (Just 5) frameInterval "" ++ " / Freq: " ++ show (recip frameInterval)
            putStrLn $ "RENDER: " ++ showFFloat (Just 5) renderInterval "" ++ " / Freq: " ++ show (recip renderInterval)

            a <- readIORef keepAlive
            when a (GLFW.pollEvents >> recur timeFrameStart)
    fix loop timeStart

    Vk.deviceWaitIdle (Pz.environmentDevice environment)

    Pz.freeRenderStateSwapchain renderer renderState
    Pz.freeSwapchainRenderTarget renderer renderTarget
    Pz.freeRenderer renderer
    Vk.destroySwapchainKHR (Pz.environmentDevice environment) swapchain Nothing
    destroySurfaceState environment surfaceState

    GLFW.destroyWindow win

    Pz.freeEnvironment environment

    GLFW.terminate



