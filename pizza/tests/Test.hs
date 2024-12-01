{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}

module Main where

-- base
import Data.Bits
import Data.Bool
import Data.Word
import Data.Maybe
import Data.Function ((&))

import Control.Monad.IO.Class
import Control.Exception (bracket)
import Control.Monad

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array

-- HUnit
import Test.HUnit

-- mtl / transformers
import Control.Monad.Trans.Cont

-- bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

-- linear
import Linear

-- vector
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

-- vulkan
import qualified Vulkan as Vk
import qualified Vulkan.Dynamic as Vk
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Zero as Vk

-- vulkan-utils
import Vulkan.Utils.Misc ((.&&.))

-- VulkanMemoryAllocator
import qualified VulkanMemoryAllocator as Vma

-- pizza
import Graphics.Pizza

main :: IO ()
main = do
    runTestTTAndExit $ TestList [
            "Patterns" ~: TestList [
                "Solid" ~: TestList [
                    "Red" ~: testPatterns (PatternSolid (V4 1 0 0 1)),
                    "Green" ~: testPatterns (PatternSolid (V4 0 1 0 1)),
                    "Blue" ~: testPatterns (PatternSolid (V4 0 0 1 1)),
                    "Grey" ~: testPatterns (PatternSolid (V4 0.5 0.5 0.5 1))
                ],
                "Linear Gradient" ~: TestList [
                    "Diagonal Red Blue" ~: testPatterns (PatternLinear (V2 0 0) (V2 200 200) (V4 1 0 0 1) (V4 0 0 0 1)),
                    "Left Right Yellow Cyan" ~: testPatterns (PatternLinear (V2 0 0) (V2 200 0) (V4 1 1 0 1) (V4 0 1 1 1)),
                    "Up Down White Black" ~: testPatterns (PatternLinear (V2 0 0) (V2 0 200) (V4 1 1 1 1) (V4 0 0 0 1))
                ],
                "Radial Gradient" ~: TestList [
                    "Center Red Black" ~: testPatterns (PatternRadial (V2 100 100) 100 (V4 1 0 0 1) (V4 0 0 0 1)),
                    "Left Up Blue Green" ~: testPatterns (PatternRadial (V2 0 0) 200 (V4 0 0 1 1) (V4 1 1 1 1))
                ]
            ],
            "Paths" ~: TestList [
                "Half" ~: testPath pathHalf maskHalf,
                "Diamond" ~: testPath pathDiamond maskDiamond,
                "Double Diamond" ~: testPath pathDoubleDiamond maskDoubleDiamond
            ]
        ]

coordinates :: [V2 Float]
coordinates = do
    y <- [0, 1 .. 199]
    x <- [0, 1 .. 199]
    pure $ V2 x y

pathFull :: Graphics.Pizza.Path
pathFull = Graphics.Pizza.Path [PathPoint (V2 0 0), PathPoint (V2 200 0), PathPoint (V2 200 200), PathPoint (V2 0 200)]

pathHalf :: Graphics.Pizza.Path
pathHalf = Graphics.Pizza.Path [PathPoint (V2 0 0), PathPoint (V2 200 0), PathPoint (V2 0 200)]

pathDiamond :: Graphics.Pizza.Path
pathDiamond = Graphics.Pizza.Path [PathPoint (V2 100 0), PathPoint (V2 200 100), PathPoint (V2 100 200), PathPoint (V2 0 100)]

pathDoubleDiamond :: Graphics.Pizza.Path
pathDoubleDiamond = Graphics.Pizza.Path [
        PathPoint (V2 0 100),
        PathPoint (V2 50 0),
        PathPoint (V2 150 200),
        PathPoint (V2 200 100),
        PathPoint (V2 150 0),
        PathPoint (V2 50 200)
    ]

maskHalf :: [Bool]
maskHalf = contains <$> coordinates
  where
    contains (V2 x y) = y <= (200 - x)

maskDiamond :: [Bool]
maskDiamond = contains <$> coordinates
  where
    contains (V2 x y)
        | x < 100   = (100 - x <= y) && (y <= 100 + x)
        | otherwise = (x - 100 <= y) && (y <= 300 - x)

maskDoubleDiamond :: [Bool]
maskDoubleDiamond = contains <$> coordinates
  where
    contains (V2 x y)
        | x < 50    = (100 - 2 * x <= y) && (y <= 100 + 2 * x)
        | x < 100   = (2 * x - 100 <= y) && (y <= 300 - 2 * x)
        | x < 150   = (300 - 2 * x <= y) && (y <= (-100) + 2 * x)
        | otherwise = (2 * x - 300 <= y) && (y <= 500 - 2 * x)

convertColor :: V4 Float -> V4 Word8
convertColor c = floor . min 255 . (* 256) <$> c

makeExpectedImage :: Pattern -> [V4 Word8]
makeExpectedImage (PatternSolid c) = replicate 40000 (convertColor c)
makeExpectedImage (PatternLinear ps pe cs ce) = let
    color p = let
        disp = p - ps
        size = pe - ps
        npos = dot disp size / dot size size
        nposc = max 0 . min 1 $ npos
        in convertColor $ lerp nposc cs ce
    in fmap color coordinates
makeExpectedImage (PatternRadial pc r cs ce) = let
    color p = let
        npos = distance p pc / r
        nposc = max 0 . min 1 $ npos
        in convertColor $ lerp nposc cs ce
    in fmap color coordinates

checkImages :: [V4 Word8] -> [V4 Word8] -> IO Bool
checkImages a b = do
    let ai = fmap fromIntegral <$> a :: [V4 Int]
        bi = fmap fromIntegral <$> b :: [V4 Int]
        r = zipWith (-) ai bi
        rs = sum . sum $ fmap abs r
        rn = length r
        good = rs < (16 * rn)

    putStrLn $ "Sum of Difference: " ++ show rs
    putStrLn $ "  Difference per pixel: " ++ show (rs `div` rn)

    unless good $ do
        print a
        putStrLn "\n"
        print b

    pure good

testPatterns :: Pattern -> Assertion
testPatterns pattern = do
    let graphics = Graphics pathFull pattern
    actual <- makeRenderedImage graphics
    let expected = makeExpectedImage pattern
    assert $ checkImages actual expected

testPath :: Graphics.Pizza.Path -> [Bool] -> Assertion
testPath path mask = do
    let graphics = Graphics path (PatternSolid (V4 1 1 1 1))
    actual <- makeRenderedImage graphics
    let expected = bool (V4 0 0 0 255) (V4 255 255 255 255) <$> mask
    assert $ checkImages actual expected

makeRenderedImage :: Graphics -> IO [V4 Word8]
makeRenderedImage graphics = evalContT do
    -- Make use of ContT, with bracket.
    -- This will make such items to be freed at out of 'scope'.

    -- Environment
    environment <- ContT $ bracket newBasicEnvironment freeEnvironment
    let Environment {..} = environment

    let format = Vk.FORMAT_R8G8B8A8_UNORM

    -- Staging Buffer
    (staging, stagingAlloc, _) <- ContT $ Vma.withBuffer environmentAllocator
        -- Vk.BufferCreateInfo []
        Vk.zero {
            Vk.size = fromIntegral (200 * 200 * sizeOf (undefined :: V4 Word8)),
            Vk.usage = Vk.BUFFER_USAGE_TRANSFER_DST_BIT,
            Vk.sharingMode = Vk.SHARING_MODE_EXCLUSIVE,
            Vk.queueFamilyIndices = V.singleton environmentGraphicsQFI
        }
        -- Vma.AllocationCreateInfo
        Vk.zero {
            Vma.flags = Vma.ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT,
            Vma.usage = Vma.MEMORY_USAGE_AUTO
        }
        bracket

    -- Pizzas
    renderer <- ContT $ bracket
        (newRenderer environment format Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL)
        freeRenderer

    renderTarget <- ContT $ bracket
        (newRenderTarget renderer 200 200 format)
        (freeRenderTarget renderer)

    liftIO $ putStrLn "Pizza Preparation"

    renderState <- ContT $ bracket
        (newRenderState renderer)
        (freeRenderState renderer)

    liftIO $ putStrLn "Pizza Render State"


    -- Command Pool
    commandPool <- ContT $ Vk.withCommandPool environmentDevice
        -- Vk.CommandPoolCreateInfo
        Vk.zero { Vk.queueFamilyIndex = environmentGraphicsQFI }
        Nothing bracket

    commandBuffers <- ContT $ Vk.withCommandBuffers environmentDevice
        -- Vk.CommandBufferAllocateInfo
        Vk.zero {
            Vk.commandPool = commandPool,
            Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY,
            Vk.commandBufferCount = 1
        }
        bracket

    let commandBuffer = V.head commandBuffers

    liftIO $ Vk.useCommandBuffer commandBuffer Vk.zero {
        Vk.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
    } $ do
        Vk.cmdCopyImageToBuffer commandBuffer
            (renderTargetImage renderTarget) Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
            staging
            (V.singleton $ Vk.zero {
                Vk.imageSubresource = Vk.zero {
                    Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT,
                    Vk.layerCount = 1
                },
                Vk.imageExtent = Vk.Extent3D 200 200 1
            })



    renderSem <- ContT $ Vk.withSemaphore environmentDevice Vk.zero Nothing bracket

    transferFence <- ContT $ Vk.withFence environmentDevice Vk.zero Nothing bracket

    renderRenderStateTarget renderer renderState graphics renderTarget Nothing

    Vk.queueSubmit environmentGraphicsQueue
        (V.singleton $ Vk.SomeStruct Vk.zero {
            Vk.waitSemaphores = V.singleton (renderStateSemaphore renderState),
            Vk.waitDstStageMask = V.singleton Vk.PIPELINE_STAGE_TRANSFER_BIT,
            Vk.commandBuffers = V.singleton $ Vk.commandBufferHandle commandBuffer
        })
        transferFence

    Vk.waitForFences environmentDevice (V.singleton transferFence) True maxBound

    ptr <- ContT $ Vma.withMappedMemory environmentAllocator stagingAlloc bracket

    let pixelPtr = castPtr ptr :: Ptr (V4 Word8)

    liftIO $ peekArray (200 * 200) pixelPtr

