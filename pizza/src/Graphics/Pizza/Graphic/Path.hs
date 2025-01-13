{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Pizza.Graphic.Path where

import Graphics.Pizza.Internal.Geometry
import Graphics.Pizza.Graphic.Curve as Curve
import Graphics.Pizza.Graphic.Transform

import Linear hiding (rotate)

data PathPart
    = PathPoint (V2 Float)
    | PathCurve Curve

instance Transformable PathPart where
  transform t (PathPoint p) = PathPoint (runTransform t p)
  transform t (PathCurve c) = PathCurve (transform t c)

  translate t (PathPoint p) = PathPoint (t + p)
  translate t (PathCurve c) = PathCurve (translate t c)

  rotate r (PathPoint p) = PathPoint (rm !* p)
    where
      rm = rotMat r
  rotate r (PathCurve c) = PathCurve (rotate r c)

  scale s (PathPoint p) = PathPoint (s * p)
  scale s (PathCurve c) = PathCurve (scale s c)

pathPartStart :: PathPart -> V2 Float
pathPartStart (PathPoint p) = p
pathPartStart (PathCurve (Curve pos _)) = pos 0

pathPartEnd :: PathPart -> V2 Float
pathPartEnd (PathPoint p) = p
pathPartEnd (PathCurve (Curve pos _)) = pos 1

newtype Path = Path [PathPart] deriving (Semigroup, Monoid)

instance Transformable Path where
  transform t (Path ps) = Path (transform t <$> ps)
  translate t (Path ps) = Path (translate t <$> ps)
  rotate r (Path ps) = Path (rotate r <$> ps)
  scale s (Path ps) = Path (scale s <$> ps)

-- Splits

data PathSplitOptions = PathSplitOptions {
    pathSplitDistance :: Float,
    pathSplitHeight :: Float
}

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
pathPartToPoints = pathPartToPoints' (PathSplitOptions 10 2)

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


-- Paths

polygon :: [V2 Float] -> Path
polygon vs = Path $ fmap PathPoint vs

circle :: V2 Float -> Float -> Path
circle center radius = Path [ PathCurve $ arc center radius 0 (2 * pi) ]



