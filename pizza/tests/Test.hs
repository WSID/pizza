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
import Graphics.Pizza.Internal.Util
import Graphics.Pizza.Internal.Geometry

-- test internal
import TestRendering

main :: IO ()
main = do
    runTestTTAndExit $ TestList [
            "Poly" ~: TestList [
                "Run" ~: TestList [
                    "0" ~: runPoly (Poly [5]) 7 ~?= 5,
                    "1" ~: runPoly (Poly [3, 1]) 7 ~?= 10,
                    "2" ~: runPoly (Poly [1, 2, 1]) 7 ~?= 64
                ],
                "FromFunc" ~: TestList [
                    "0" ~: polyFromFunc (const 5) ~?= Poly [5],
                    "1" ~: polyFromFunc (+ 3) ~?= Poly [3, 1],
                    "2" ~: polyFromFunc (\x -> x*x + 2 * x + 1) ~?= Poly [1, 2, 1]
                ],
                "Multiplicaiton" ~: TestList [
                    "1 x 2" ~: do
                        let a = Poly [2, 1]
                            b = Poly [1, 2, 1]
                        runPoly (a * b) 3 ~?= runPoly a 3 * runPoly b 3,
                    "2 x 3" ~: do
                        let a = Poly [3, 3, 1]
                            b = Poly [1, 2, 3, 4]
                        runPoly (a * b) 7 ~?= runPoly a 7 * runPoly b 7
                ]
            ],
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
