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

import Data.Vector (Vector)
import qualified Data.Vector as V

import qualified Graphics.UI.GLFW as GLFW

import qualified Vulkan as Vk

import qualified Graphics.Pizza as Pz

import Demo

-- time
import Data.Time.Clock

type MyFormat = Pz.VBGRA (Pz.UNorm Word8)

data DeviceSelection = DeviceSelection {
    devSelectionScore :: Int,
    devSelectionGraphicQueueFamilyIndex :: Int
}

deviceSelection :: Vk.Instance -> Vector BSC.ByteString -> Vk.PhysicalDevice -> IO (Maybe DeviceSelection)
deviceSelection inst reqExts device = do
    -- Device
    _ <- Vk.getPhysicalDeviceProperties device -- Currently we don't check this anyway.

    hasExts <- Pz.checkDeviceExtensions reqExts device
    hasColorBlendEquation <- Pz.checkExtendedDynamicState3Features device

    if hasExts && hasColorBlendEquation then do

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

    else do
        pure Nothing

pickDevice :: Vk.Instance -> Vector BSC.ByteString -> Vector Vk.PhysicalDevice -> IO (Vk.PhysicalDevice, Word32)
pickDevice inst reqExts pdevices = do
    pdevicesWithScore <- flip V.mapMaybeM pdevices $ \pdevice -> do
        selection <- deviceSelection inst reqExts pdevice
        pure $ fmap (\s -> (s, pdevice)) selection

    when (null pdevicesWithScore) $ die "No suitable device!"

    let (selection, pdevice) = V.maximumOn (devSelectionScore . fst) pdevicesWithScore
    pure (pdevice, fromIntegral $ devSelectionGraphicQueueFamilyIndex selection)

createEnvironment :: IO Pz.Environment
createEnvironment = do
    instExtensionsGLFW <- GLFW.getRequiredInstanceExtensions
    instExtensionsVK <- traverse BS.packCString instExtensionsGLFW

    let envOpt = Pz.EnvOptSimple {
        Pz.envOptInstExtensions = instExtensionsVK,
        Pz.envOptInstLayers = [BSC.pack "VK_LAYER_KHRONOS_validation"],
        Pz.envOptPickDevice = pickDevice,
        Pz.envOptDevExtensions = [Vk.KHR_SWAPCHAIN_EXTENSION_NAME],
        Pz.envOptDevLayers = [],
        Pz.envOptAppName = Just $ BSC.pack "pizza-preview-window",
        Pz.envOptAppVersion = 0
    }

    Pz.newEnvironmentSimple envOpt


createSurface :: Pz.Environment -> GLFW.Window -> IO Vk.SurfaceKHR
createSurface Pz.Environment {..} window = do
    let instGlfw = castPtr $ Vk.instanceHandle environmentInst

    alloca $ \surfacePtr -> do
        res <- GLFW.createWindowSurface instGlfw window nullPtr (castPtr surfacePtr) :: IO Int32
        when (Vk.Result res /= Vk.SUCCESS) $ die ("Cannot create surface from window: Code = " ++ show res)
        peek surfacePtr


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
    surface <- createSurface environment win
    renderCore <- Pz.newRenderCore environment

    renderer <- Pz.newRenderer renderCore Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR :: IO (Pz.Renderer (Pz.VBGRA (Pz.UNorm Word8)))
    mayRenderTarget <- Pz.newSurfaceRenderTarget renderCore renderer surface width height
    let renderTarget = case mayRenderTarget of
            Just swapchain -> swapchain
            Nothing -> error "Cannot find format."
    renderState <- Pz.newRenderStateSwapchain renderCore

    demo <- makeBasicDemo renderCore 100 100

    keepAlive <- newIORef True
    GLFW.setWindowCloseCallback win $ Just (\_ -> writeIORef keepAlive False)
    GLFW.showWindow win

    timeStart <- getCurrentTime

    let loop recur timePrevFrame = do
            timeFrameStart <- getCurrentTime
            let timeDiff = realToFrac $ diffUTCTime timeFrameStart timeStart
            let graphics = demoGraphic demo timeDiff

            (_, presentWait) <- Pz.renderRenderStateTargetSurface renderCore renderer renderState graphics renderTarget
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

    demoFree demo

    Pz.freeRenderStateSwapchain renderCore renderState
    Pz.freeSurfaceRenderTarget renderCore renderTarget
    Pz.freeRenderer renderer
    Pz.freeRenderCore renderCore
    Vk.destroySurfaceKHR (Pz.environmentInst environment) surface Nothing

    GLFW.destroyWindow win

    Pz.freeEnvironment environment

    GLFW.terminate

