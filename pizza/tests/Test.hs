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
import Graphics.Pizza.Graphic.Curve

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
            "Graphics" ~: TestList [
                "CurveRunner" ~: TestList [
                    "Simple Case" ~: do
                        let curve = lineCurve (V2 0 0) (V2 100 0)
                        let r0 = mkCurveRunner curve 16
                        let CurveRunning t1 r1 = runCurveRunner r0 10
                        let CurveRunning t2 r2 = runCurveRunner r1 30
                        (t1, t2) ~?= (0.1, 0.4),

                    "Arc" ~: do
                        let PathCurve curve = arc (V2 0 0) 100 0 2
                        let r0 = mkCurveRunner curve 256
                        let CurveRunning t1 r1 = runCurveRunner r0 20
                        let CurveRunning t2 r2 = runCurveRunner r1 40
                        assertBool "t1 ~= 0.1" (abs (t1 - 0.1) < 0.001)
                        assertBool "t2 ~= 0.3" (abs (t2 - 0.3) < 0.001)
                ],
                "DashPattern" ~: TestList [
                    "start on" ~: TestList [
                        "none" ~: dashPatternStartOn dashPatternNone ~?= False,
                        "full" ~: dashPatternStartOn dashPatternFull ~?= True,
                        "1" ~: dashPatternStartOn (DashPattern True [10, 20]) ~?= True,
                        "2" ~: dashPatternStartOn (DashPattern False [10, 20, 30]) ~?= False
                    ],
                    "end on" ~: TestList [
                        "none" ~: dashPatternEndOn dashPatternNone ~?= False,
                        "full" ~: dashPatternEndOn dashPatternFull ~?= True,
                        "1" ~: dashPatternEndOn (DashPattern True [10, 20]) ~?= True,
                        "2" ~: dashPatternEndOn (DashPattern False [10, 20, 30]) ~?= True
                    ],
                    "cons" ~: TestList [
                        "none" ~: dashPatternCons dashPatternNone ~?= (False, Nothing),
                        "full" ~: dashPatternCons dashPatternFull ~?= (True, Nothing),
                        "1" ~: dashPatternCons (DashPattern True [10, 20]) ~?= (True, Just (10, DashPattern False [20])),
                        "2" ~: dashPatternCons (DashPattern False [10, 20, 30]) ~?= (False, Just (10, DashPattern True [20, 30]))
                    ]
                ]
            ],
            testTreeRendering
        ]

lineCurve :: V2 Float -> V2 Float -> Curve
lineCurve start end = Curve {
  curvePosition = \t -> lerp t start end,
  curveDirection = const (end - start)
}
