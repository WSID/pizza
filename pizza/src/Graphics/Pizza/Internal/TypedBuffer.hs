{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExplicitForAll #-}

module Graphics.Pizza.Internal.TypedBuffer where

import Control.Monad.IO.Class

import Data.Foldable

import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Array

-- vector
import qualified Data.Vector as V

-- vulkan
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk

-- VulkanMemoryAllocator
import qualified VulkanMemoryAllocator as Vma


-- pizza
import Graphics.Pizza.Renderer


-- | Typed Buffer
data TypedBuffer a = TypedBuffer {
    typedBufferAlloc :: Vma.Allocation,
    typedBufferObject :: Vk.Buffer
}

newTypedBufferSized :: (MonadIO m) => Renderer -> Vk.BufferUsageFlags -> Int -> m (TypedBuffer a)
newTypedBufferSized Renderer {..} usage size = do
    let Environment {..} = rendererEnvironment

    (typedBufferObject, typedBufferAlloc, _) <- Vma.createBuffer
        environmentAllocator
        Vk.zero { -- Vk.BufferCreateInfo
            Vk.size = fromIntegral size,
            Vk.usage = usage,
            Vk.sharingMode = Vk.SHARING_MODE_EXCLUSIVE,
            Vk.queueFamilyIndices = V.singleton environmentGraphicsQFI
        }
        Vk.zero { -- Vma.AllocationCreateInfo
            Vma.flags = Vma.ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT,
            Vma.usage = Vma.MEMORY_USAGE_AUTO
        }

    pure TypedBuffer {..}


newTypedBufferNA :: (MonadIO m, Storable a) => Renderer -> Vk.BufferUsageFlags -> Int -> a -> m (TypedBuffer a)
newTypedBufferNA renderer usage n a = newTypedBufferSized renderer usage (n * sizeOf a)

newTypedBufferN :: (MonadIO m, Storable a) => Renderer -> Vk.BufferUsageFlags -> Int -> m (TypedBuffer a)
newTypedBufferN renderer usage n = newTypedBufferNA renderer usage n undefined

newTypedBufferF :: (MonadIO m, Foldable f, Storable a) => Renderer -> Vk.BufferUsageFlags -> f a -> m (TypedBuffer a)
newTypedBufferF renderer usage f = do
    let n = length f
    typedBuffer <- newTypedBufferN renderer usage n

    ptr <- mapTypedBuffer renderer typedBuffer
    liftIO $ pokeArray ptr (toList f)
    unmapTypedBuffer renderer typedBuffer

    pure typedBuffer

castTypedBuffer :: TypedBuffer a -> TypedBuffer b
castTypedBuffer TypedBuffer {..} = TypedBuffer {..}


freeTypedBuffer :: (MonadIO m) => Renderer -> TypedBuffer a -> m ()
freeTypedBuffer Renderer {..} TypedBuffer {..} =
    Vma.destroyBuffer environmentAllocator typedBufferObject typedBufferAlloc
  where
    Environment {..} = rendererEnvironment


mapTypedBuffer :: (MonadIO m) => Renderer -> TypedBuffer a -> m (Ptr a)
mapTypedBuffer Renderer {..} TypedBuffer {..} =
    castPtr <$> Vma.mapMemory environmentAllocator typedBufferAlloc
  where
    Environment {..} = rendererEnvironment

unmapTypedBuffer :: (MonadIO m) => Renderer -> TypedBuffer a -> m ()
unmapTypedBuffer Renderer {..} TypedBuffer {..} =
    Vma.unmapMemory environmentAllocator typedBufferAlloc
  where
    Environment {..} = rendererEnvironment


readTypedBuffer1 :: (MonadIO m, Storable a) => Renderer -> TypedBuffer a -> m a
readTypedBuffer1 renderer typedBuffer = do
    ptr <- mapTypedBuffer renderer typedBuffer
    result <- liftIO $ peek ptr
    unmapTypedBuffer renderer typedBuffer
    pure result

writeTypedBuffer1 :: (MonadIO m, Storable a) => Renderer -> TypedBuffer a -> a -> m ()
writeTypedBuffer1 renderer typedBuffer a = do
    ptr <- mapTypedBuffer renderer typedBuffer
    liftIO $ poke ptr a
    unmapTypedBuffer renderer typedBuffer
