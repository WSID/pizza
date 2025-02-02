{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}

module Graphics.Pizza.Device.Environment where

import Control.Monad.IO.Class

import Data.Bits
import Data.List
import Data.Word

import Foreign.Ptr

-- vector
import Data.Vector (Vector)
import qualified Data.Vector as V

-- bytestring
import qualified Data.ByteString.Char8 as BSC

-- vulkan
import qualified Vulkan as Vk
import Vulkan.CStruct.Extends (pattern (::&), pattern (:&))
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Dynamic as Vk
import qualified Vulkan.Zero as Vk

-- VulkanMemoryAllocator
import qualified VulkanMemoryAllocator as Vma


-- Constants

-- | A set of required VkInstance extensions.
--
-- If you need to make VkInstance for your own, include this to create new instance.
requiredInstanceExtensions :: [BSC.ByteString]
requiredInstanceExtensions = [
        Vk.KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
    ]

requiredDeviceExtensions :: [BSC.ByteString]
requiredDeviceExtensions = [
        Vk.EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME,
        Vk.EXT_EXTENDED_DYNAMIC_STATE_3_EXTENSION_NAME
    ]

data Environment = Environment {
    environmentInst :: Vk.Instance,
    environmentPhysDevice :: Vk.PhysicalDevice,
    environmentDevice :: Vk.Device,
    environmentGraphicsQFI :: Word32,
    environmentGraphicsQueue :: Vk.Queue,
    environmentAllocator :: Vma.Allocator
}

data EnvOptSimple = EnvOptSimple {
    envOptInstExtensions :: [BSC.ByteString],
    envOptInstLayers :: [BSC.ByteString],
    envOptPickDevice :: Vk.Instance -> Vector BSC.ByteString -> Vector Vk.PhysicalDevice -> IO (Vk.PhysicalDevice, Word32),
    envOptDevExtensions :: [BSC.ByteString],
    envOptDevLayers :: [BSC.ByteString],
    envOptAppName :: Maybe BSC.ByteString,
    envOptAppVersion :: Word32
}

defaultEnvOptSimple :: EnvOptSimple
defaultEnvOptSimple = EnvOptSimple {
    envOptInstExtensions = [],
    envOptInstLayers = [],
    envOptPickDevice = defaultPickDevice,
    envOptDevExtensions = [],
    envOptDevLayers = [],
    envOptAppName = Nothing,
    envOptAppVersion = 0
}

checkDeviceExtensions :: Vector BSC.ByteString -> Vk.PhysicalDevice -> IO Bool
checkDeviceExtensions reqExts physDevice = do
    (_, devExtProps) <- Vk.enumerateDeviceExtensionProperties physDevice Nothing
    let devExts = fmap (\Vk.ExtensionProperties { Vk.extensionName = name} -> name) devExtProps
    pure $ V.all (`elem` devExts) reqExts

checkExtendedDynamicState3Features :: Vk.PhysicalDevice -> IO Bool
checkExtendedDynamicState3Features physDevice = do
    Vk.PhysicalDeviceFeatures2 {} ::&
        Vk.PhysicalDeviceExtendedDynamicState3FeaturesEXT {
            Vk.extendedDynamicState3ColorBlendAdvanced = colorBlendAdv
        } :&
        ()
        <- Vk.getPhysicalDeviceFeatures2KHR physDevice
    
    pure colorBlendAdv

defaultDeviceSuitable ::  Vector BSC.ByteString -> Vk.PhysicalDevice -> IO (Maybe (Vk.PhysicalDevice, Word32))
defaultDeviceSuitable reqExts physDevice = do
    hasExts <- checkDeviceExtensions reqExts physDevice
    hasColorBlendEquation <- checkExtendedDynamicState3Features physDevice
    if hasExts && hasColorBlendEquation then do
        qprops <- Vk.getPhysicalDeviceQueueFamilyProperties physDevice

        let queueCriteria Vk.QueueFamilyProperties {..} =
                queueFlags .&. Vk.QUEUE_GRAPHICS_BIT /= zeroBits


        pure ((\a -> (physDevice, a)) . fromIntegral <$> V.findIndex queueCriteria qprops)
    else do
        pure Nothing

defaultPickDevice :: Vk.Instance -> Vector BSC.ByteString -> Vector Vk.PhysicalDevice -> IO (Vk.PhysicalDevice, Word32)
defaultPickDevice _ reqExts physDevices = do
    physDeviceGraphicsQFIs <- V.mapMaybeM (defaultDeviceSuitable reqExts) physDevices
    pure $ V.head physDeviceGraphicsQFIs


-- | Create a basic environment for vector graphics.
newBasicEnvironment :: MonadIO m => m Environment
newBasicEnvironment = newEnvironmentSimple defaultEnvOptSimple

newEnvironmentSimple :: MonadIO m => EnvOptSimple -> m Environment
newEnvironmentSimple EnvOptSimple {..} = do

    let instExtensions = V.fromList (requiredInstanceExtensions `union` envOptInstExtensions)
    let devExtensions = V.fromList (requiredDeviceExtensions `union` envOptDevExtensions)

    environmentInst <- Vk.createInstance Vk.zero {
        Vk.applicationInfo = Just Vk.ApplicationInfo {
            Vk.applicationName = envOptAppName,
            Vk.applicationVersion = envOptAppVersion,
            Vk.engineName = Just $ BSC.pack "pizza",
            Vk.engineVersion = Vk.MAKE_API_VERSION 0 1 0,
            Vk.apiVersion = Vk.API_VERSION_1_0
        },
        Vk.enabledLayerNames = V.fromList envOptInstLayers,
        Vk.enabledExtensionNames = instExtensions
    } Nothing

    (_, physDevices) <- Vk.enumeratePhysicalDevices environmentInst

    (environmentPhysDevice, environmentGraphicsQFI) <- liftIO
        $ envOptPickDevice environmentInst devExtensions physDevices

    environmentDevice <- Vk.createDevice
        environmentPhysDevice
        (
            Vk.zero {
                Vk.queueCreateInfos = V.singleton $ Vk.SomeStruct Vk.zero {
                    Vk.queueFamilyIndex = environmentGraphicsQFI,
                    Vk.queuePriorities = V.singleton 1.0
                },
                Vk.enabledLayerNames = V.fromList envOptDevLayers,
                Vk.enabledExtensionNames = devExtensions
            } ::&
            (Vk.zero :: Vk.PhysicalDeviceExtendedDynamicState3FeaturesEXT) {
                Vk.extendedDynamicState3ColorBlendAdvanced = True
            } :&
            ()
        )
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


