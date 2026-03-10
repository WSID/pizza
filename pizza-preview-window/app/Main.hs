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
import Data.Foldable

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

import System.Exit

import Numeric

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Data.Vector (Vector)
import qualified Data.Vector as V

import qualified Graphics.UI.GLFW as GLFW

import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import qualified Vulkan.CStruct.Extends as Vk

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


makeDemos :: Pz.RenderCore -> IO [Demo]
makeDemos renderCore = sequence
    [ makeBasicDemo renderCore 100 100
    , makeGraphDemo
    , makeRoadDemo
    ]


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
    renderState <- Pz.newRenderState renderCore
    
    imageSem <- Vk.createSemaphore (Pz.environmentDevice environment) Vk.zero Nothing
    renderFence <- Vk.createFence (Pz.environmentDevice environment) Vk.zero Nothing

    keepAlive <- newIORef True
    GLFW.setWindowCloseCallback win $ Just (\_ -> writeIORef keepAlive False)
    GLFW.showWindow win

    demos <- makeDemos renderCore

    timeStart <- getCurrentTime
    demoRef <- newIORef (0, timeStart)

    -- Command Buffer
    cmdpool <- Vk.createCommandPool
        (Pz.environmentDevice environment)
        Vk.CommandPoolCreateInfo {
            Vk.flags =
                Vk.COMMAND_POOL_CREATE_TRANSIENT_BIT .|.
                Vk.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
            Vk.queueFamilyIndex = Pz.environmentGraphicsQFI environment
        }
        Nothing

    cmdbufs <- Vk.allocateCommandBuffers
        (Pz.environmentDevice environment)
        Vk.CommandBufferAllocateInfo {
            Vk.commandPool = cmdpool,
            Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY,
            Vk.commandBufferCount = 1
        }

    let cmdbuf = case V.toList cmdbufs of
            [cmd] -> cmd
            _ -> error "Unexpected count of command buffers"

    GLFW.setKeyCallback win $ Just $ \_ key _scancode _keyState _mod -> do
        when (_keyState == GLFW.KeyState'Released) $ do
            let demoCount = length demos
            (demoIndex, _) <- readIORef demoRef

            let demoIndexPred = pred (if demoIndex /= 0 then demoIndex else demoCount)
            let demoIndexSucc = let ni = succ demoIndex in if ni == demoCount then 0 else ni

            let mayNextDemoIndex = case key of
                    GLFW.Key'A -> Just demoIndexPred
                    GLFW.Key'Left -> Just demoIndexPred
                    GLFW.Key'D -> Just demoIndexSucc
                    GLFW.Key'Right -> Just demoIndexSucc
                    _ -> Nothing

            for_ mayNextDemoIndex $ \nextDemoIndex -> do
                demoTimeStart <- getCurrentTime
                writeIORef demoRef (nextDemoIndex, demoTimeStart)


    let loop recur timePrevFrame = do
            timeFrameStart <- getCurrentTime
            (demoIndex, demoTimeStart) <- readIORef demoRef
            let timeDiff = realToFrac $ diffUTCTime timeFrameStart demoTimeStart
            let graphics = demoGraphic (demos !! demoIndex) timeDiff
            
            Vk.resetFences
                (Pz.environmentDevice environment)
                (V.singleton renderFence)

            
            (index, baseRenderTarget, renderSem) <- Pz.surfaceRenderTargetAcquireNext
                renderCore
                renderTarget
                (Just imageSem)

            _ <- Vk.useCommandBuffer
                cmdbuf
                Vk.CommandBufferBeginInfo {
                    Vk.next = (),
                    Vk.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
                    Vk.inheritanceInfo = Nothing
                } $ do
                Pz.setRenderStateTargetBase
                    cmdbuf
                    renderCore
                    renderer
                    renderState
                    graphics
                    400
                    400
                    baseRenderTarget

            Vk.queueSubmit
                (Pz.environmentGraphicsQueue environment)
                (V.singleton $ Vk.SomeStruct Vk.zero {
                    Vk.waitSemaphores = V.singleton imageSem,
                    Vk.waitDstStageMask = V.singleton Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
                    Vk.commandBuffers = V.singleton (Vk.commandBufferHandle cmdbuf),
                    Vk.signalSemaphores = V.singleton renderSem
                } )
                renderFence
                
            Pz.surfaceRenderTargetPresent
                renderCore
                renderTarget
                index
                
            timeFrameDone <- getCurrentTime
            
            _ <- Vk.waitForFences
                (Pz.environmentDevice environment)
                (V.singleton renderFence)
                True
                maxBound

            let frameInterval = realToFrac $ diffUTCTime timeFrameStart timePrevFrame :: Float
            let renderInterval = realToFrac $ diffUTCTime timeFrameDone timeFrameStart :: Float

            putStrLn $ "FRAME: " ++ showFFloat (Just 5) frameInterval "" ++ " / Freq: " ++ show (recip frameInterval)
            putStrLn $ "RENDER: " ++ showFFloat (Just 5) renderInterval "" ++ " / Freq: " ++ show (recip renderInterval)

            a <- readIORef keepAlive
            when a (GLFW.pollEvents >> recur timeFrameStart)
    fix loop timeStart

    Vk.deviceWaitIdle (Pz.environmentDevice environment)

    Vk.destroyCommandPool (Pz.environmentDevice environment) cmdpool Nothing

    for_ demos demoFree
    
    Vk.destroySemaphore (Pz.environmentDevice environment) imageSem Nothing
    Vk.destroyFence (Pz.environmentDevice environment) renderFence Nothing

    Pz.freeRenderState renderCore renderState
    Pz.freeSurfaceRenderTarget renderCore renderTarget
    Pz.freeRenderer renderer
    Pz.freeRenderCore renderCore
    Vk.destroySurfaceKHR (Pz.environmentInst environment) surface Nothing

    GLFW.destroyWindow win

    Pz.freeEnvironment environment

    GLFW.terminate

