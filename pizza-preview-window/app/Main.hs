module Main where

import Control.Monad

import Data.Foldable
import Data.IORef
import Data.Function

import qualified Graphics.UI.GLFW as GLFW

main :: IO ()
main = do
    initSucc <- GLFW.init
    when initSucc $ do
        keepAlive <- newIORef True
        mwin <- GLFW.createWindow 400 400 "Pizza Preview" Nothing Nothing
        for_ mwin $ \win -> do
            GLFW.setWindowCloseCallback win $ Just (\_ -> writeIORef keepAlive False)
            GLFW.showWindow win

            fix $ \recur -> do
                a <- readIORef keepAlive
                when a (GLFW.waitEvents >> recur)
        GLFW.terminate
