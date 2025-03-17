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
    renderCore <- newRenderCore environment

    renderer <- newRenderer renderCore Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL :: IO (Renderer (VRGBA (UNorm Word8)))
    renderTarget <- newRenderTarget renderCore renderer 200 200
    renderState <- newRenderState renderCore
    exchange <- newExchangeN renderCore (200 * 200)

    -- Render image from graphic. It happens on GPU.
    _ <- renderRenderStateTarget renderCore renderer renderState graphics renderTarget Nothing

    -- Wait image to exchange, to access pixels in CPU.
    join $ writeExchangeRenderTarget renderCore exchange renderTarget (Just (renderStateSemaphore renderState))
    _ <- Vk.waitForFences environmentDevice (V.singleton (exchangeFence exchange)) True maxBound

    -- Copy image pixels from exchange.
    result <- readExchangeN renderCore exchange (200 * 200)

    -- Wrap up & clean up
    freeExchange renderCore exchange
    freeRenderState renderCore renderState
    freeRenderTarget renderCore renderTarget
    freeRenderer renderer
    freeRenderCore renderCore

    freeEnvironment environment

    pure (fmap (fmap getUNorm . unVRGBA) result)
