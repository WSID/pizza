{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
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
import Graphics.Pizza.Device.Environment

-- | Typed Buffer
data TypedBuffer a = TypedBuffer {
    typedBufferAlloc :: Vma.Allocation,
    typedBufferObject :: Vk.Buffer
}

newTypedBufferSized :: (MonadIO m) => Environment -> Vk.BufferUsageFlags -> Int -> m (TypedBuffer a)
newTypedBufferSized Environment {..} usage size = do
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


newTypedBufferNA :: (MonadIO m, Storable a) => Environment -> Vk.BufferUsageFlags -> Int -> a -> m (TypedBuffer a)
newTypedBufferNA environment usage n a = newTypedBufferSized environment usage (n * sizeOf a)

newTypedBufferN :: (MonadIO m, Storable a) => Environment -> Vk.BufferUsageFlags -> Int -> m (TypedBuffer a)
newTypedBufferN environment usage n = newTypedBufferNA environment usage n undefined

newTypedBufferF :: (MonadIO m, Foldable f, Storable a) => Environment -> Vk.BufferUsageFlags -> f a -> m (TypedBuffer a)
newTypedBufferF environment usage f = do
    let n = length f
    typedBuffer <- newTypedBufferN environment usage n

    ptr <- mapTypedBuffer environment typedBuffer
    liftIO $ pokeArray ptr (toList f)
    unmapTypedBuffer environment typedBuffer

    pure typedBuffer

castTypedBuffer :: TypedBuffer a -> TypedBuffer b
castTypedBuffer TypedBuffer {..} = TypedBuffer {..}


freeTypedBuffer :: (MonadIO m) => Environment -> TypedBuffer a -> m ()
freeTypedBuffer Environment {..} TypedBuffer {..} =
    Vma.destroyBuffer environmentAllocator typedBufferObject typedBufferAlloc


mapTypedBuffer :: (MonadIO m) => Environment -> TypedBuffer a -> m (Ptr a)
mapTypedBuffer Environment {..} TypedBuffer {..} =
    castPtr <$> Vma.mapMemory environmentAllocator typedBufferAlloc

unmapTypedBuffer :: (MonadIO m) => Environment -> TypedBuffer a -> m ()
unmapTypedBuffer Environment {..} TypedBuffer {..} =
    Vma.unmapMemory environmentAllocator typedBufferAlloc

withTypedBufferMap :: (MonadIO m) => Environment -> TypedBuffer a -> (Ptr a -> m b) -> m b
withTypedBufferMap environment typedBuffer action = do 
    ptr <- mapTypedBuffer environment typedBuffer
    result <- action ptr
    unmapTypedBuffer environment typedBuffer
    pure result

readTypedBuffer1 :: (MonadIO m, Storable a) => Environment -> TypedBuffer a -> m a
readTypedBuffer1 environment typedBuffer = withTypedBufferMap environment typedBuffer $ liftIO . peek

writeTypedBuffer1 :: (MonadIO m, Storable a) => Environment -> TypedBuffer a -> a -> m ()
writeTypedBuffer1 environment typedBuffer a = withTypedBufferMap environment typedBuffer $ \p -> liftIO (poke p a)

readTypedBufferN :: (MonadIO m, Storable a) => Environment -> TypedBuffer a -> Int -> m [a]
readTypedBufferN environment typedBuffer n = withTypedBufferMap environment typedBuffer $ \p -> liftIO (peekArray n p)

writeTypedBufferN :: (MonadIO m, Storable a) => Environment -> TypedBuffer a -> [a] -> m ()
writeTypedBufferN environment typedBuffer a = withTypedBufferMap environment typedBuffer $ \p -> liftIO (pokeArray p a)