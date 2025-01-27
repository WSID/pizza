{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}

module ImageRender where

-- base
import Data.Word

import Foreign.Marshal.Array

-- linear
import Linear

-- vector
import qualified Data.Vector as V

-- vulkan
import qualified Vulkan as Vk

-- pizza
import Graphics.Pizza

makeRenderedImage :: Graphics -> IO [V4 Word8]
makeRenderedImage graphics = do

    -- Environment
    environment <- newBasicEnvironment
    let Environment {..} = environment

    let format = Vk.FORMAT_R8G8B8A8_UNORM

    -- Pizzas
    renderer <- newRenderer environment format Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
    renderTarget <- newRenderTarget renderer 200 200 format
    renderState <- newRenderState renderer
    exchange <- newExchangeN renderer (200 * 200)

    -- Render image from graphic. It happens on GPU.
    _ <- renderRenderStateTarget renderer renderState graphics renderTarget Nothing

    -- Wait image to exchange, to access pixels in CPU.
    writeExchangeRenderTarget renderer exchange renderTarget (Just (renderStateSemaphore renderState))
    _ <- Vk.waitForFences environmentDevice (V.singleton (exchangeFence exchange)) True maxBound

    -- Copy image pixels from exchange.
    result <- readExchangeN renderer exchange (200 * 200)

    -- Wrap up & clean up
    freeExchange renderer exchange
    freeRenderState renderer renderState
    freeRenderTarget renderer renderTarget
    freeRenderer renderer

    freeEnvironment environment

    pure result
