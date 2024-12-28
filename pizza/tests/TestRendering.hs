module TestRendering where

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

-- test internals
import ImageRender

testTreeRendering :: Test
testTreeRendering = "Rendering" ~: TestList [
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
            "Double Diamond" ~: testPath pathDoubleDiamond maskDoubleDiamond,
            "Half Circle" ~: testPath pathHalfCircle maskHalfCircle,
            "Circle" ~: testPath pathCircle maskCircle,
            "Corner Bezier" ~: testPath pathCornerBezier maskCornerBezier,
            "Outline Circle" ~: testPath pathOutlineCircle maskOutlineCircle,
            "Strokes Join" ~: TestList [
                "Miter" ~: testPath pathJoinMiter maskJoinMiter,
                "Round" ~: testPath pathJoinRound maskJoinRound,
                "Bevel" ~: testPath pathJoinBevel maskJoinBevel
            ],
            "Dash" ~: TestList [
                "Line 1" ~: testPath pathDashLine1 maskDashLine1,
                "Line 2" ~: testPath pathDashLine2 maskDashLine2,
                "Curve 1" ~: testPath pathDashCurve1 maskDashCurve1,
                "Curve 2" ~: testPath pathDashCurve2 maskDashCurve2
            ]
        ]
    ]

coordinates :: [V2 Float]
coordinates = do
    y <- [0, 1 .. 199]
    x <- [0, 1 .. 199]
    pure $ V2 x y

-- Paths

pathFull :: Graphics.Pizza.Path
pathFull = Graphics.Pizza.Path [
        PathPoint (V2 0 0),
        PathPoint (V2 200 0),
        PathPoint (V2 200 200),
        PathPoint (V2 0 200)
    ]

pathHalf :: [Graphics.Pizza.Path]
pathHalf = [
        Graphics.Pizza.Path [
            PathPoint (V2 0 0),
            PathPoint (V2 200 0),
            PathPoint (V2 0 200)
        ]
    ]

pathDiamond :: [Graphics.Pizza.Path]
pathDiamond = [
        Graphics.Pizza.Path [
            PathPoint (V2 100 0),
            PathPoint (V2 200 100),
            PathPoint (V2 100 200),
            PathPoint (V2 0 100)
        ]
    ]

pathDoubleDiamond :: [Graphics.Pizza.Path]
pathDoubleDiamond = [
        Graphics.Pizza.Path [
            PathPoint (V2 0 100),
            PathPoint (V2 50 0),
            PathPoint (V2 150 200),
            PathPoint (V2 200 100),
            PathPoint (V2 150 0),
            PathPoint (V2 50 200)
        ]
    ]

pathHalfCircle :: [Graphics.Pizza.Path]
pathHalfCircle = [Graphics.Pizza.Path [ arc (V2 100 100) 100 0 pi ]]

pathCircle :: [Graphics.Pizza.Path]
pathCircle = [Graphics.Pizza.Path [ arc (V2 100 100) 100 0 (2 * pi) ]]

pathCornerBezier :: [Graphics.Pizza.Path]
pathCornerBezier = [
        Graphics.Pizza.Path [
            PathPoint (V2 0 0),
            bezier (V2 200 0) [V2 200 200] (V2 0 200)
        ]
    ]

pathOutlineCircle :: [Graphics.Pizza.Path]
pathOutlineCircle = stroke (StrokeOption 20 strokeJoinMiter) True $ Graphics.Pizza.Path [ arc (V2 100 100) 90 0 (2 * pi) ]

joinTestPath :: Graphics.Pizza.Path
joinTestPath = Graphics.Pizza.Path [
        PathPoint (V2 0 50),
        PathPoint (V2 150 50),
        PathPoint (V2 150 200)
    ]

pathJoinMiter :: [Graphics.Pizza.Path]
pathJoinMiter = stroke (StrokeOption 100 strokeJoinMiter) False joinTestPath

pathJoinRound :: [Graphics.Pizza.Path]
pathJoinRound = stroke (StrokeOption 100 strokeJoinRound) False joinTestPath

pathJoinBevel :: [Graphics.Pizza.Path]
pathJoinBevel = stroke (StrokeOption 100 strokeJoinBevel) False joinTestPath

pathDashLine1 :: [Graphics.Pizza.Path]
pathDashLine1 = stroke (StrokeOption 100 strokeJoinMiter) False =<<
    dash (DashPattern True [50, 50, 50])
        (Graphics.Pizza.Path [PathPoint (V2 0 100), PathPoint (V2 200 100)])

pathDashLine2 :: [Graphics.Pizza.Path]
pathDashLine2 = stroke (StrokeOption 100 strokeJoinMiter) False =<<
    dash (DashPattern False [50, 50])
        (Graphics.Pizza.Path [PathPoint (V2 0 100), PathPoint (V2 200 100)])

pathDashCurve1 :: [Graphics.Pizza.Path]
pathDashCurve1 = stroke (StrokeOption 100 strokeJoinMiter) False =<<
    dash (DashPattern True [50, 50, 50])
        (Graphics.Pizza.Path [arc (V2 0 0) 150 0 (0.5 * pi)])

pathDashCurve2 :: [Graphics.Pizza.Path]
pathDashCurve2 = stroke (StrokeOption 20 strokeJoinMiter) False =<<
    dash (DashPattern True [50, 100, 100, 100, 50])
        (Graphics.Pizza.Path
            [
                arc (V2 100 100) 100 (1 - pi) (- pi),
                arc (V2 100 100) 100 (0) (1)
            ]
        )

-- Masks

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

maskHalfCircle :: [Bool]
maskHalfCircle = contains <$> coordinates
  where
    contains (V2 x y) = (distance (V2 x y) (V2 100 100) <= 100) && (y >= 100)

maskCircle :: [Bool]
maskCircle = contains <$> coordinates
  where
    contains coord = distance coord (V2 100 100) <= 100

maskCornerBezier :: [Bool]
maskCornerBezier = contains <$> coordinates
  where
    contains (V2 x y) = let t = 1 - (sqrt (160000 - 800 * y) / 400) in (x <= 200 * (1 - t * t))

maskOutlineCircle :: [Bool]
maskOutlineCircle = contains <$> coordinates
  where
    contains coord = let dist = distance coord (V2 100 100) in (80 <= dist) && (dist <= 100)

maskJoinMiter :: [Bool]
maskJoinMiter = contains <$> coordinates
  where
    contains (V2 x y) = ((100 <= x) && (x <= 200)) || ((0 <= y) && (y <= 100))

maskJoinRound :: [Bool]
maskJoinRound = contains <$> coordinates
  where
    inTop (V2 x y) = (0 <= y) && (y <= 100) && (0 <= x) && (x <= 150)
    inRight (V2 x y) = (100 <= x) && (x <= 200) && (50 <= y) && (y <= 200)
    inRound pos = distance pos (V2 150 50) <= 50
    contains pos = inTop pos || inRight pos || inRound pos


maskJoinBevel :: [Bool]
maskJoinBevel = contains <$> coordinates
  where
    contains (V2 x y) =
        (((100 <= x) && (x <= 200)) || ((0 <= y) && (y <= 100))) && (x - y <= 150)

maskDashLine1 :: [Bool]
maskDashLine1 = contains <$> coordinates
  where
    contains (V2 x y) =
        (
            ((0 <= x) && (x <= 50)) ||
            ((100 <= x) && (x <= 150))
        )
        && ((50 <= y) && (y <= 150))

maskDashLine2 :: [Bool]
maskDashLine2 = contains <$> coordinates
  where
    contains (V2 x y) =
        ((50 <= x) && (x <= 100))
        && ((50 <= y) && (y <= 150))

maskDashCurve1 :: [Bool]
maskDashCurve1 = contains <$> coordinates
  where
    contains p = let r = norm p; t150 = unangle p * 150 in
        ((100 <= r) && (r <= 200)) &&
        (
            ((0 <= t150) && (t150 <= 50)) ||
            ((100 <= t150) && (t150 <= 150))
        )

maskDashCurve2 :: [Bool]
maskDashCurve2 = contains <$> coordinates
  where
    contains p = let
        V2 x y = p
        d = p  - V2 100 100
        r = norm d
        t100 = unangle d * 100
        in
        (
            ((50 <= x) && (x <= 150) && (90 <= y) && (y <= 110)) ||
            (
                (90 <= r) && (r <= 110) &&
                (
                    ((50 <= t100) && (t100 <= 100)) ||
                    ((100 * pi + 50 <= t100 ) && (t100 <= 100 * pi + 100))
                )
            )
        )

-- Test utility

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
    let graphics = Graphics [pathFull] pattern
    actual <- makeRenderedImage graphics
    let expected = makeExpectedImage pattern
    assert $ checkImages actual expected

testPath :: [Graphics.Pizza.Path] -> [Bool] -> Assertion
testPath paths mask = do
    let graphics = Graphics paths (PatternSolid (V4 1 1 1 1))
    actual <- makeRenderedImage graphics
    let expected = bool (V4 0 0 0 255) (V4 255 255 255 255) <$> mask
    assert $ checkImages actual expected
