{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}

module ImageRender where

-- base
import Control.Monad
import Data.Word

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

    -- Pizzas
    renderer <- newRenderer environment Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL :: IO (Renderer (VRGBA (UNorm Word8)))
    renderTarget <- newRenderTarget renderer 200 200
    renderState <- newRenderState renderer
    exchange <- newExchangeN renderer (200 * 200)

    -- Render image from graphic. It happens on GPU.
    _ <- renderRenderStateTarget renderer renderState graphics renderTarget noImageSet Nothing

    -- Wait image to exchange, to access pixels in CPU.
    join $ writeExchangeRenderTarget renderer exchange renderTarget (Just (renderStateSemaphore renderState))
    _ <- Vk.waitForFences environmentDevice (V.singleton (exchangeFence exchange)) True maxBound

    -- Copy image pixels from exchange.
    result <- readExchangeN renderer exchange (200 * 200)

    -- Wrap up & clean up
    freeExchange renderer exchange
    freeRenderState renderer renderState
    freeRenderTarget renderer renderTarget
    freeRenderer renderer

    freeEnvironment environment

    pure (fmap (fmap getUNorm . unVRGBA) result)
