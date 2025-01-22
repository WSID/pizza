{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}

module Graphics.Pizza.Environment where

import Control.Monad.IO.Class

import Data.Bits
import Data.Traversable
import Data.Word

import Foreign.Ptr

-- vector
import Data.Vector (Vector)
import qualified Data.Vector as V

-- bytestring
import qualified Data.ByteString.Char8 as BSC

-- vulkan
import qualified Vulkan as Vk
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Dynamic as Vk
import qualified Vulkan.Zero as Vk

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

data EnvOptSimple = EnvOptSimple {
    envOptInstExtensions :: [BSC.ByteString],
    envOptInstLayers :: [BSC.ByteString],
    envOptPickDevice :: Vk.Instance -> Vector Vk.PhysicalDevice -> IO (Vk.PhysicalDevice, Word32),
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

defaultPickDevice :: Vk.Instance -> Vector Vk.PhysicalDevice -> IO (Vk.PhysicalDevice, Word32)
defaultPickDevice _ physDevices = do
    physDeviceGraphicsQFIs <- for physDevices $ \physDevice -> do
        qprops <- Vk.getPhysicalDeviceQueueFamilyProperties physDevice

        let queueCriteria Vk.QueueFamilyProperties {..} =
                queueFlags .&. Vk.QUEUE_GRAPHICS_BIT /= zeroBits

        pure $ fromIntegral <$> V.findIndex queueCriteria qprops

    pure
        $ V.head
        $ V.mapMaybe (\(a, b) -> (,) <$> Just a <*> b)
        $ V.zip physDevices physDeviceGraphicsQFIs


-- | Create a basic environment for vector graphics.
newBasicEnvironment :: MonadIO m => m Environment
newBasicEnvironment = newEnvironmentSimple defaultEnvOptSimple

newEnvironmentSimple :: MonadIO m => EnvOptSimple -> m Environment
newEnvironmentSimple EnvOptSimple {..} = do
    environmentInst <- Vk.createInstance Vk.zero {
        Vk.applicationInfo = Just Vk.ApplicationInfo {
            Vk.applicationName = envOptAppName,
            Vk.applicationVersion = envOptAppVersion,
            Vk.engineName = Just $ BSC.pack "pizza",
            Vk.engineVersion = Vk.MAKE_API_VERSION 0 1 0,
            Vk.apiVersion = Vk.API_VERSION_1_0
        },
        Vk.enabledLayerNames = V.fromList envOptInstLayers,
        Vk.enabledExtensionNames = V.fromList envOptInstExtensions
    } Nothing

    (_, physDevices) <- Vk.enumeratePhysicalDevices environmentInst

    (environmentPhysDevice, environmentGraphicsQFI) <- liftIO
        $ envOptPickDevice environmentInst physDevices

    environmentDevice <- Vk.createDevice
        environmentPhysDevice
        Vk.zero {
            Vk.queueCreateInfos = V.singleton $ Vk.SomeStruct Vk.zero {
                Vk.queueFamilyIndex = environmentGraphicsQFI,
                Vk.queuePriorities = V.singleton 1.0
            },
            Vk.enabledLayerNames = V.fromList envOptDevLayers,
            Vk.enabledExtensionNames = V.fromList envOptDevExtensions
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


