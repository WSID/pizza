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
    result <- renderToList (V2 200 200) graphics
    pure (fmap (fmap getUNorm . unVRGBA) result)
