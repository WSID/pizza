{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}

module Main where

-- base
import Data.Bool
import Data.Word

import Control.Monad

-- HUnit
import Test.HUnit

-- linear
import Linear

-- pizza
import Graphics.Pizza
import Graphics.Pizza.Internal.Geometry

-- test internal
import TestRendering

main :: IO ()
main = do
    runTestTTAndExit $ TestList [
            "Geometry" ~: TestList [
                "LineIntersect" ~: TestList [
                    "Parallel" ~: do
                        lineIntersect (V2 2 2) (V2 1 1) (V2 0 0) (V2 1 1) ~?= Nothing,
                    "Obvious Cases" ~: do
                        lineIntersect (V2 1 3) (V2 1 0) (V2 1 3) (V2 0 1) ~?= Just (V2 1 3),
                    "Axes" ~: do
                        lineIntersect (V2 0 0) (V2 1 0) (V2 0 0) (V2 0 1) ~?= Just (V2 0 0),
                    "1" ~: do
                        lineIntersect (V2 0 0) (V2 2 1) (V2 (-2) 3) (V2 2 (-1)) ~?= Just (V2 2 1)
                ]
            ],
            testTreeRendering
        ]
