{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Pizza (
    module Graphics.Pizza.Device.Environment,
    module Graphics.Pizza.Device.Format,
    module Graphics.Pizza.Device.Image,
    module Graphics.Pizza.Device.Renderer,
    module Graphics.Pizza.Device.RenderCore,
    module Graphics.Pizza.Device.RenderState,
    module Graphics.Pizza.Device.RenderTarget,
    module Graphics.Pizza.Graphic,
    module Graphics.Pizza.Painting,
    module Graphics.Pizza.Internal.TypedBuffer,
    renderToListOf,
    renderToList
) where

-- base
import Control.Monad.IO.Class
import Data.Bits

-- linear
import Linear

-- vector
import qualified Data.Vector as V

-- vulkan
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import qualified Vulkan.CStruct.Extends as Vk

-- pizza
import Graphics.Pizza.Device.Environment
import Graphics.Pizza.Device.Format
import Graphics.Pizza.Device.Image
import Graphics.Pizza.Device.Renderer
import Graphics.Pizza.Device.RenderCore
import Graphics.Pizza.Device.RenderState
import Graphics.Pizza.Device.RenderTarget
import Graphics.Pizza.Graphic
import Graphics.Pizza.Painting

import Graphics.Pizza.Internal.TypedBuffer


-- Facade

-- | Renders graphics into a single list.
renderToListOf :: (MonadIO m, Format px) => V2 Int -> Graphics -> px -> m [px]
renderToListOf (V2 width height) graphics px = do
    -- Environment
    environment <- newBasicEnvironment
    let Environment {..} = environment

    -- Command Buffer
    cmdpool <- Vk.createCommandPool
        environmentDevice
        Vk.CommandPoolCreateInfo {
            Vk.flags = Vk.COMMAND_POOL_CREATE_TRANSIENT_BIT,
            Vk.queueFamilyIndex = environmentGraphicsQFI
        }
        Nothing

    cmdbufs <- Vk.allocateCommandBuffers
        environmentDevice
        Vk.CommandBufferAllocateInfo {
            Vk.commandPool = cmdpool,
            Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY,
            Vk.commandBufferCount = 1
        }

    let cmdbuf = case V.toList cmdbufs of
            [cmd] -> cmd
            _ -> error "Unexpected count of command buffers"

    -- Vulkan Synchronization
    fence <- Vk.createFence
        environmentDevice
        Vk.FenceCreateInfo {
            Vk.next = (),
            Vk.flags = zeroBits
        }
        Nothing


    renderCore <- newRenderCore environment
    renderer <- newRendererOf renderCore Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL px
    renderTarget <- newRenderTarget renderCore renderer width height
    renderState <- newRenderState renderCore

    staging <- newTypedBufferN
        environment
        Vk.BUFFER_USAGE_TRANSFER_DST_BIT
        (width * height)

    Vk.useCommandBuffer
        cmdbuf
        Vk.CommandBufferBeginInfo {
            Vk.next = (),
            Vk.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
            Vk.inheritanceInfo = Nothing
        } $ do
            setRenderStateTargetBase
                cmdbuf
                renderCore
                renderer
                renderState
                graphics
                width
                height
                (renderTargetBase renderTarget)

            Vk.cmdCopyImageToBuffer cmdbuf
                (renderTargetImage renderTarget)
                Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
                (typedBufferObject staging)
                (V.singleton Vk.BufferImageCopy {
                    Vk.bufferOffset = 0,
                    Vk.bufferRowLength = 0,
                    Vk.bufferImageHeight = 0,
                    Vk.imageSubresource = Vk.ImageSubresourceLayers {
                        Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT,
                        Vk.mipLevel = 0,
                        Vk.baseArrayLayer = 0,
                        Vk.layerCount = 1
                    },
                    Vk.imageOffset = Vk.Offset3D 0 0 0,
                    Vk.imageExtent = Vk.Extent3D
                        (fromIntegral width) (fromIntegral height) 1
                })

    Vk.queueSubmit
        environmentGraphicsQueue
        (V.singleton $ Vk.SomeStruct Vk.zero {
            Vk.commandBuffers = V.singleton $ Vk.commandBufferHandle cmdbuf
        } )
        fence

    _ <- Vk.waitForFences environmentDevice (V.singleton fence) True maxBound

    -- Copy image pixels from exchange.
    result <- readTypedBufferN environment staging (width * height)

    -- Wrap up & clean up
    freeTypedBuffer environment staging
    freeRenderState renderCore renderState
    freeRenderTarget renderCore renderTarget
    freeRenderer renderer
    freeRenderCore renderCore

    Vk.destroyCommandPool environmentDevice cmdpool Nothing

    freeEnvironment environment

    pure result

renderToList :: (MonadIO m, Format px) => V2 Int -> Graphics -> m [px]
renderToList size graphics = renderToListOf size graphics undefined