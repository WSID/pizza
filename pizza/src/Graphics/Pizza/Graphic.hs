{-# LANGUAGE RecordWildCards #-}

module Graphics.Pizza.Graphic where

import Control.Applicative

-- linear
import Linear

import Graphics.Pizza.Internal.Util

data Graphics = Graphics [Path] Pattern

data Pattern =
    PatternSolid (V4 Float) |
    PatternLinear (V2 Float) (V2 Float) (V4 Float) (V4 Float) |
    PatternRadial (V2 Float) Float (V4 Float) (V4 Float)


data AABB = AABB !(V2 Float) !(V2 Float)

data PathPart
    = PathPoint (V2 Float)
    | PathCurve (Float -> V2 Float)

newtype Path = Path [PathPart]

data PathSplitOptions = PathSplitOptions {
    pathSplitDistance :: Float,
    pathSplitHeight :: Float
}

-- Functions

continueSplitPoint :: PathSplitOptions -> V2 Float -> V2 Float -> V2 Float -> Bool
continueSplitPoint PathSplitOptions {..} a b c = metDist || metHeight
  where
    ac = c - a
    bc = c - b
    dist = norm ac
    crs = abs (crossZ bc ac)

    metDist = pathSplitDistance < dist
    metHeight
        | dist < 0.001  = pathSplitHeight < norm bc
        | otherwise = pathSplitHeight * dist < crs


pathPartToPoints :: PathPart -> [V2 Float]
pathPartToPoints p = pathPartToPoints' (PathSplitOptions 10 2) p

pathPartToPoints' :: PathSplitOptions -> PathPart -> [V2 Float]
pathPartToPoints' _ (PathPoint a) = [a]
pathPartToPoints' opt (PathCurve f) = [f 0] ++ go 0 (f 0) 1 (f 1) ++ [f 1]
  where
    go start sv end ev
        | continue = go start sv m mv ++ [mv] ++ go m mv end ev
        | otherwise = [mv]
      where
        m = (end + start) * 0.5
        mv = f m
        continue = continueSplitPoint opt sv mv ev

pathToPoints :: Path -> [V2 Float]
pathToPoints (Path p) = p >>= pathPartToPoints

-- Utility

rangeDivList :: Int -> [Float]
rangeDivList n = fmap (\i -> fromIntegral i / nf) [0 .. n]
    where
        nf = fromIntegral n

-- Curves

bezier :: V2 Float -> [V2 Float] -> V2 Float -> PathPart
bezier start controls end = PathCurve $ \t -> runVPoly t (bezierPoly start controls end)

arc :: V2 Float -> Float -> Float -> Float -> PathPart
arc center radius start end = PathCurve $ \t -> center + angle (start + (end - start) * t) ^* radius


-- Path to points

bezierPoly :: V2 Float -> [V2 Float] -> V2 Float -> VPoly
bezierPoly start [] end = VPoly [start, end - start]
bezierPoly start controls end = result
    where
        a = bezierPoly start (init controls) (last controls)
        b = bezierPoly (head controls) (tail controls) end
        VPoly bf = subVPoly b a
        result = addVPoly a (VPoly (zero : bf))
