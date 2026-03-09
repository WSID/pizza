{-# LANGUAGE RecordWildCards #-}

module Graphics.Pizza (
    module Graphics.Pizza.Device.Environment,
    module Graphics.Pizza.Device.Exchange,
    module Graphics.Pizza.Device.Format,
    module Graphics.Pizza.Device.Image,
    module Graphics.Pizza.Device.Renderer,
    module Graphics.Pizza.Device.RenderCore,
    module Graphics.Pizza.Device.RenderState,
    module Graphics.Pizza.Device.RenderTarget,
    module Graphics.Pizza.Graphic,
    module Graphics.Pizza.Painting,
    
    renderToList
) where

-- base
import Control.Monad
import Control.Monad.IO.Class

-- linear
import Linear

-- vector
import qualified Data.Vector as V 

-- vulkan
import qualified Vulkan as Vk

-- pizza
import Graphics.Pizza.Device.Environment
import Graphics.Pizza.Device.Exchange
import Graphics.Pizza.Device.Format
import Graphics.Pizza.Device.Image
import Graphics.Pizza.Device.Renderer
import Graphics.Pizza.Device.RenderCore
import Graphics.Pizza.Device.RenderState
import Graphics.Pizza.Device.RenderTarget
import Graphics.Pizza.Graphic
import Graphics.Pizza.Painting


-- Facade

-- | Renders graphics into a single list.
renderToList :: (MonadIO m, Format px) => V2 Int -> Graphics -> m [px]
renderToList (V2 width height) graphics = do
    -- Environment
    environment <- newBasicEnvironment
    let Environment {..} = environment

    -- Pizzas
    renderCore <- newRenderCore environment
    
    renderer <- newRenderer renderCore Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
    renderTarget <- newRenderTarget renderCore renderer width height
    renderState <- newRenderState renderCore
    exchange <- newExchangeN renderCore (width * height)

    -- Render image from graphic. It happens on GPU.
    _ <- renderRenderStateTarget renderCore renderer renderState graphics renderTarget Nothing

    -- Wait image to exchange, to access pixels in CPU.
    _ <- join $ writeExchangeRenderTarget renderCore exchange renderTarget (Just (renderStateSemaphore renderState))
    _ <- Vk.waitForFences environmentDevice (V.singleton (exchangeFence exchange)) True maxBound

    -- Copy image pixels from exchange.
    result <- readExchangeN renderCore exchange (width * height)

    -- Wrap up & clean up
    freeExchange renderCore exchange
    freeRenderState renderCore renderState
    freeRenderTarget renderCore renderTarget
    freeRenderer renderer
    freeRenderCore renderCore

    freeEnvironment environment

    pure result