module TestRendering where

-- base
import Data.List
import Data.Bool
import Data.Word

import Control.Monad

-- HUnit
import Test.HUnit

-- linear
import Linear hiding (transpose)

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
            "Strokes Cap" ~: TestList [
                "None" ~: testPath pathCapNone maskCapNone,
                "Square" ~: testPath pathCapSquare maskCapSquare,
                "Round" ~: testPath pathCapRound maskCapRound
            ],
            "Dash" ~: TestList [
                "Line 1" ~: testPath pathDashLine1 maskDashLine1,
                "Line 2" ~: testPath pathDashLine2 maskDashLine2,
                "Curve 1" ~: testPath pathDashCurve1 maskDashCurve1,
                "Curve 2" ~: testPath pathDashCurve2 maskDashCurve2,
                "Box" ~: testPath pathDashBox maskDashBox,
                "Box2" ~: testPath pathDashBox2 maskDashBox2
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
pathHalfCircle = [Graphics.Pizza.Path [ PathCurve $ arc (V2 100 100) 100 0 pi ]]

pathCircle :: [Graphics.Pizza.Path]
pathCircle = [Graphics.Pizza.Path [ PathCurve $ arc (V2 100 100) 100 0 (2 * pi) ]]

pathCornerBezier :: [Graphics.Pizza.Path]
pathCornerBezier = [
        Graphics.Pizza.Path [
            PathPoint (V2 0 0),
            PathCurve $ bezier (V2 200 0) [V2 200 200] (V2 0 200)
        ]
    ]

pathOutlineCircle :: [Graphics.Pizza.Path]
pathOutlineCircle = stroke (StrokeOption 20 strokeJoinMiter) StrokeClose
    (Graphics.Pizza.Path [
        PathCurve $ arc (V2 100 100) 90 0 (2 * pi)
    ])

joinTestPath :: Graphics.Pizza.Path
joinTestPath = Graphics.Pizza.Path [
        PathPoint (V2 0 50),
        PathPoint (V2 150 50),
        PathPoint (V2 150 200)
    ]

pathJoinMiter :: [Graphics.Pizza.Path]
pathJoinMiter = stroke (StrokeOption 100 strokeJoinMiter) strokeEndNone joinTestPath

pathJoinRound :: [Graphics.Pizza.Path]
pathJoinRound = stroke (StrokeOption 100 strokeJoinRound) strokeEndNone joinTestPath

pathJoinBevel :: [Graphics.Pizza.Path]
pathJoinBevel = stroke (StrokeOption 100 strokeJoinBevel) strokeEndNone joinTestPath


capTestPath :: Graphics.Pizza.Path
capTestPath = Graphics.Pizza.Path [
        PathPoint (V2 50 100),
        PathPoint (V2 150 100)
    ]

pathCapNone :: [Graphics.Pizza.Path]
pathCapNone = stroke (StrokeOption 100 strokeJoinMiter) strokeEndNone capTestPath

pathCapSquare :: [Graphics.Pizza.Path]
pathCapSquare = stroke (StrokeOption 100 strokeJoinMiter) (strokeEndBoth strokeCapSquare) capTestPath

pathCapRound :: [Graphics.Pizza.Path]
pathCapRound = stroke (StrokeOption 100 strokeJoinMiter) (strokeEndBoth strokeCapRound) capTestPath

pathDashLine1 :: [Graphics.Pizza.Path]
pathDashLine1 = dashStroke
    (DashPattern True [50, 50, 50])
    (StrokeOption 100 strokeJoinMiter)
    strokeEndNone
    (Graphics.Pizza.Path [PathPoint (V2 0 100), PathPoint (V2 200 100)])

pathDashLine2 :: [Graphics.Pizza.Path]
pathDashLine2 = dashStroke
    (DashPattern False [50, 50])
    (StrokeOption 100 strokeJoinMiter)
    strokeEndNone
    (Graphics.Pizza.Path [PathPoint (V2 0 100), PathPoint (V2 200 100)])

pathDashCurve1 :: [Graphics.Pizza.Path]
pathDashCurve1 = dashStroke
    (DashPattern True [50, 50, 50])
    (StrokeOption 100 strokeJoinMiter)
    strokeEndNone
    (Graphics.Pizza.Path [PathCurve $ arc (V2 0 0) 150 0 (0.5 * pi)])

pathDashCurve2 :: [Graphics.Pizza.Path]
pathDashCurve2 = dashStroke
    (DashPattern True [50, 100, 100, 100, 50])
    (StrokeOption 20 strokeJoinMiter)
    strokeEndNone
    (Graphics.Pizza.Path
        [
            PathCurve $ arc (V2 100 100) 100 (1 - pi) (- pi),
            PathCurve $ arc (V2 100 100) 100 (0) (1)
        ]
    )

pathDashBox :: [Graphics.Pizza.Path]
pathDashBox = dashStroke
    (DashPattern False [60, 60, 120, 60, 120, 60, 120, 60])
    (StrokeOption 20 strokeJoinMiter)
    StrokeClose
    (Graphics.Pizza.Path
        [
            PathPoint (V2 10 10),
            PathPoint (V2 190 10),
            PathPoint (V2 190 190),
            PathPoint (V2 10 190)
        ]
    )

pathDashBox2 :: [Graphics.Pizza.Path]
pathDashBox2 = dashStroke
    (DashPattern True [60, 60, 120, 60, 120, 60, 120, 60])
    (StrokeOption 20 strokeJoinMiter)
    StrokeClose
    (Graphics.Pizza.Path
        [
            PathPoint (V2 10 10),
            PathPoint (V2 190 10),
            PathPoint (V2 190 190),
            PathPoint (V2 10 190)
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
        | x < 100   = inRange (100 - x) (100 + x) y
        | otherwise = inRange (x - 100) (300 - x) y

maskDoubleDiamond :: [Bool]
maskDoubleDiamond = contains <$> coordinates
  where
    contains (V2 x y)
        | x < 50    = inRange (100 - x2) (100 + x2) y
        | x < 100   = inRange (x2 - 100) (300 - x2) y
        | x < 150   = inRange (300 - x2) ((-100) + x2) y
        | otherwise = inRange (x2 - 300) (500 - x2) y
      where
        x2 = x * 2

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
    contains coord = inRange 80 100 (distance coord (V2 100 100))

maskJoinMiter :: [Bool]
maskJoinMiter = contains <$> coordinates
  where
    contains (V2 x y) = inRange 100 200 x || inRange 0 100 y

maskJoinRound :: [Bool]
maskJoinRound = contains <$> coordinates
  where
    inTop = inBox (V2 0 0) (V2 150 100)
    inRight = inBox (V2 100 50) (V2 200 200)
    inRound pos = distance pos (V2 150 50) <= 50
    contains pos = inTop pos || inRight pos || inRound pos


maskJoinBevel :: [Bool]
maskJoinBevel = contains <$> coordinates
  where
    contains (V2 x y) =
        (inRange 100 200 x || inRange 0 100 y) && (x - y <= 150)

maskCapNone :: [Bool]
maskCapNone = inBox (V2 50 50) (V2 150 150) <$> coordinates

maskCapSquare :: [Bool]
maskCapSquare = inBox (V2 0 50) (V2 200 150) <$> coordinates

maskCapRound :: [Bool]
maskCapRound = contains <$> coordinates
  where
    contains p = (distance p (V2 50 100) <= 50) || inBox (V2 50 50) (V2 150 150) p || (distance p (V2 150 100) <= 50)

maskDashLine1 :: [Bool]
maskDashLine1 = contains <$> coordinates
  where
    contains (V2 x y) = (inRange 0 50 x || inRange 100 150 x) && inRange 50 150 y

maskDashLine2 :: [Bool]
maskDashLine2 = contains <$> coordinates
  where
    contains = inBox (V2 50 50) (V2 100 150)

maskDashCurve1 :: [Bool]
maskDashCurve1 = contains <$> coordinates
  where
    contains p = let r = norm p; t150 = unangle p * 150 in
        inRange 100 200 r && (inRange 0 50 t150 || inRange 100 150 t150)

maskDashCurve2 :: [Bool]
maskDashCurve2 = contains <$> coordinates
  where
    contains p = let
        d = p  - V2 100 100
        r = norm d
        t100 = unangle d * 100
        in
        inBox (V2 50 90) (V2 150 110) p ||
        (
            inRange 90 110 r &&
            (inRange 50 100 t100 || inRange (100 * pi + 50) (100 * pi + 100) t100)
        )

maskDashBox :: [Bool]
maskDashBox = contains <$> coordinates
  where
    contains p = or [
            inBox (V2 0 70) (V2 20 130) p,
            inBox (V2 70 0) (V2 130 20) p,
            inBox (V2 180 70) (V2 200 130) p,
            inBox (V2 70 180) (V2 130 200) p
        ]

maskDashBox2 :: [Bool]
maskDashBox2 = contains <$> coordinates
  where
    contains p = not (
            inBox (V2 20 20) (V2 180 180) p ||
            inBox (V2 70 0) (V2 130 200) p ||
            inBox (V2 0 70) (V2 200 130) p
        )

-- Test utility

inRange :: (Ord a) => a -> a -> a -> Bool
inRange s e v = (min s e <= v) && (v <= max s e)

inBox :: (Ord a) => V2 a -> V2 a -> V2 a -> Bool
inBox s e v = and (inRange <$> s <*> e <*> v)

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

    putStrLn ""
    putStrLn ("  Sum of Difference: " ++ show rs)
    putStrLn ("  Difference per pixel: " ++ show (rs `div` rn))

    unless good $ do
        printDiff (zipWith (==) ai bi)

    pure good

testPatterns :: Pattern -> Assertion
testPatterns pattern = do
    let attrs = DrawAttributes {
        drawPattern = pattern,
        drawTransform = mempty,
        drawBlend = BlendNormal,
        drawOpacity = 1.0
    }
    let graphics = Graphics [ DrawShape [pathFull] attrs]
    actual <- makeRenderedImage graphics
    let expected = makeExpectedImage pattern
    assert $ checkImages actual expected

testPath :: [Graphics.Pizza.Path] -> [Bool] -> Assertion
testPath paths mask = do
    let attrs = DrawAttributes {
        drawPattern = PatternSolid (V4 1 1 1 1),
        drawTransform = mempty,
        drawBlend = BlendNormal,
        drawOpacity = 1.0
    }
    let graphics = Graphics [ DrawShape paths attrs ]
    actual <- makeRenderedImage graphics
    let expected = bool (V4 0 0 0 255) (V4 255 255 255 255) <$> mask
    assert $ checkImages actual expected

batched :: Int -> [a] -> [[a]]
batched _ [] = []
batched n a = let (h, t) = splitAt n a in h : batched n t

printDiff :: [Bool] -> IO ()
printDiff good = do
    let rows = batched 20 $ and <$> batched 10 good
        cols = transpose rows
        res = transpose $ fmap (fmap and . batched 10) cols
    putStrLn $ unlines $ fmap ((++ "|") . fmap (\g -> if g then ' ' else 'X')) res



