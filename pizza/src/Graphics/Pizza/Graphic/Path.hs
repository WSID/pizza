{-# LANGUAGE RecordWildCards #-}

module Graphics.Pizza.Graphic.Path where

import Graphics.Pizza.Graphic.Curve as Curve
import Graphics.Pizza.Internal.Util

import Linear

data PathPart
    = PathPoint (V2 Float)
    | PathCurve Curve

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
pathPartToPoints' opt (PathCurve (Curve f _)) = [f 0] ++ go 0 (f 0) 1 (f 1) ++ [f 1]
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

-- Curves

bezier :: V2 Float -> [V2 Float] -> V2 Float -> PathPart
bezier start controls end = PathCurve . fromVPoly $ bezierPoly start controls end

arc :: V2 Float -> Float -> Float -> Float -> PathPart
arc center radius start end = PathCurve $ Curve {
    curvePosition = \t -> center + angle (start + (end - start) * t) ^* radius,
    curveDirection = \t -> perp $ angle (start + (end - start) * t)
}

-- Paths

polygon :: [V2 Float] -> Path
polygon vs = Path $ fmap PathPoint vs

circle :: V2 Float -> Float -> Path
circle center radius = Path [arc center radius 0 (2 * pi)]

-- Path to points

bezierPoly :: V2 Float -> [V2 Float] -> V2 Float -> VPoly
bezierPoly start [] end = VPoly [start, end - start]
bezierPoly start controls end = result
    where
        a = bezierPoly start (init controls) (last controls)
        b = bezierPoly (head controls) (tail controls) end
        VPoly bf = subVPoly b a
        result = addVPoly a (VPoly (zero : bf))

-- outline

pathPartStart :: PathPart -> V2 Float
pathPartStart (PathPoint p) = p
pathPartStart (PathCurve (Curve pos _)) = pos 0

pathPartEnd :: PathPart -> V2 Float
pathPartEnd (PathPoint p) = p
pathPartEnd (PathCurve (Curve pos _)) = pos 1

