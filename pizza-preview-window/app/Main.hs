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

makeGraphic :: Pz.Image px -> Float -> Pz.Graphics
makeGraphic image time = let env = Pz.defPaintingEnv { Pz.paintingThickness = 10 }
    in flip Pz.runPainting_ env $ do
        let theta = time * 2
            animValue1 = 400 * cos theta
            animValue2 = 100 * sin theta

        -- Shape 1
        let pattern1 = Pz.PatternImage
                image
                (Pz.fromScale (V2 0.01 0.01))
                1

        Pz.localPattern (const pattern1) $ do
            Pz.localOpacity (const (abs (sin theta))) $ do     
                Pz.paintFillShaping $ do
                    Pz.shapingPathing $ do
                        Pz.pathingCurve $ Pz.bezier
                            (V2 0 200)
                            [ V2 0 (0 - animValue1), V2 400 (400 + animValue1) ]
                            (V2 400 200)

                        Pz.pathingCurve $ Pz.arc
                            (V2 (300 + animValue2) 200)
                            (100 - animValue2)
                            0
                            (negate pi)

                        Pz.pathingCurve $ Pz.arc
                            (V2 (100 + animValue2) 200)
                            (100 + animValue2)
                            0
                            pi
                    Pz.shapingPath () $ Pz.circle (V2 200 200) (100 + animValue2)

        -- Shape 2
        let pattern2 = Pz.PatternLinear
                (V2 (200 - 200 * cos theta) (200 + 200 * sin theta))
                (V2 (200 + 200 * cos theta) (200 - 200 * sin theta))
                (V4 0 1 0 1)
                (V4 1 0 1 1)

        Pz.localTransform (Pz.scale (pure (abs (cos theta)))) $ do
            Pz.localPattern (const pattern2) $ do
                Pz.localEndCaps (const (Pz.strokeCapRound, Pz.strokeCapRound)) $ do
                    Pz.localBlend (const Pz.BlendOverlay) $ do
                        Pz.paintStrokeOpenPathing $ do
                            Pz.pathingCurve $ Pz.arc (V2 300 100) 50 (pi * (-0.5)) (pi * (0.5))
                            Pz.pathingCurve $ Pz.arc (V2 100 100) 50 (pi * (0.5)) (pi * (1.5))

        -- Shape 3
        let pattern3 = Pz.PatternSolid (V4 0.5 0.5 0.5 1)

        Pz.localTransform (Pz.translate (V2 0 200)) $ do
            Pz.localPattern (const pattern3) $ do
                Pz.localDashPattern (const $ Just (Pz.DashPattern True (50 + 50 * sin theta : cycle [50, 50, 50, 50]))) $ do
                    Pz.localBlend (const Pz.BlendLighten) $ do 
                        Pz.paintStrokeClosePathing $ do
                            Pz.pathingCurve $ Pz.arc (V2 300 100) 50 (pi * (-0.5)) (pi * (0.5))
                            Pz.pathingCurve $ Pz.arc (V2 100 100) 50 (pi * (0.5)) (pi * (1.5))

makeWaveImage :: Pz.RenderCore -> Int -> Int -> IO (Pz.Image (Pz.VRGBA (Pz.UNorm Word8)))
makeWaveImage renderCore width height = do
    let coordinates = do
            y <- [0, 1 .. width]
            x <- [0, 1 .. height]
            pure $ V2 x y
        
        fw = fromIntegral width :: Float
        fh = fromIntegral height :: Float 
        
        mapper (V2 x y) = Pz.VRGBA $ V4 (Pz.UNorm b) (Pz.UNorm g) (Pz.UNorm r) (Pz.UNorm 255)
          where
            fx = fromIntegral x :: Float
            fy = fromIntegral y :: Float
            sp = (1 + sin (fx * pi * 2 / fw)) * 0.5
            cp = (1 + cos (fy * pi * 2 / fh)) * 0.5
            b = round (sp * 255)
            g = round (sqrt (sp * cp) * 255)
            r = round (cp * 255)
    
    exchange <- Pz.newExchangeN renderCore (width * height)
    image <- Pz.newImage (Pz.renderCoreEnvironment renderCore) width height 
    Pz.writeExchangeN renderCore exchange (fmap mapper coordinates)
    join $ Pz.copyExchangeToImage renderCore exchange image Nothing

    Pz.freeExchange renderCore exchange
    pure image


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

    image <- makeWaveImage renderCore 100 100

    keepAlive <- newIORef True
    GLFW.setWindowCloseCallback win $ Just (\_ -> writeIORef keepAlive False)
    GLFW.showWindow win

    timeStart <- getCurrentTime

    let loop recur timePrevFrame = do
            timeFrameStart <- getCurrentTime
            let timeDiff = realToFrac $ diffUTCTime timeFrameStart timeStart
            let graphics = makeGraphic image timeDiff

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

    Pz.freeImage environment image

    Pz.freeRenderStateSwapchain renderCore renderState
    Pz.freeSurfaceRenderTarget renderCore renderTarget
    Pz.freeRenderer renderer
    Pz.freeRenderCore renderCore
    Vk.destroySurfaceKHR (Pz.environmentInst environment) surface Nothing

    GLFW.destroyWindow win

    Pz.freeEnvironment environment

    GLFW.terminate

