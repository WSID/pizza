{-# LANGUAGE RecordWildCards #-}

module Graphics.Pizza.Graphic where

import Control.Applicative

-- linear
import Linear

import Graphics.Pizza.Graphic.Curve
import qualified Graphics.Pizza.Graphic.Curve as Curve
import Graphics.Pizza.Internal.Geometry
import Graphics.Pizza.Internal.Util
import Control.Arrow (ArrowChoice(right))

data Graphics = Graphics [Path] Pattern

data Pattern =
    PatternSolid (V4 Float) |
    PatternLinear (V2 Float) (V2 Float) (V4 Float) (V4 Float) |
    PatternRadial (V2 Float) Float (V4 Float) (V4 Float)


data AABB = AABB !(V2 Float) !(V2 Float)

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

-- Utility

rangeDivList :: Int -> [Float]
rangeDivList n = fmap (\i -> fromIntegral i / nf) [0 .. n]
    where
        nf = fromIntegral n

-- Curves

bezier :: V2 Float -> [V2 Float] -> V2 Float -> PathPart
bezier start controls end = PathCurve . fromVPoly $ bezierPoly start controls end

arc :: V2 Float -> Float -> Float -> Float -> PathPart
arc center radius start end = PathCurve $ Curve {
    curvePosition = \t -> center + angle (start + (end - start) * t) ^* radius,
    curveDirection = \t -> negate $ perp $ angle (start + (end - start) * t)
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

stroke :: Float -> Bool -> Path -> [Path]
stroke _ _ (Path []) = []
stroke _ _ (Path [PathPoint _]) = []

stroke thickness closed (Path [PathCurve curve])
  | closed      = [ Path [lc], Path [rc] ]
  | otherwise   = [ Path [lc, rc] ]
  where
    (lc, rc) = leftRightCurve thickness curve

stroke thickness closed (Path (p: q: ps))
  | closed      = [Path lres, Path rres]
  | otherwise   = [Path (lres <> rres)]
  where
    onPoint pos dir [] = ([PathPoint lp], [PathPoint rp])
      where
        (lp, rp) = maybe (leftRightDir thickness pos dir) (leftRightDir2 thickness pos dir) lastDir

    onPoint pos dir (cp: cps) = ([PathPoint lp] <> nextLeft, nextRight <> [PathPoint rp])
      where
        nextPos = pathPartStart cp
        nextDir = signorm (nextPos - pos)
        (lp, rp) = leftRightDir2 thickness pos dir nextDir
        (nextLeft, nextRight) = onIter nextDir cp cps

    onIter prevDir (PathPoint a) rest = onPoint a prevDir rest
    onIter prevDir (PathCurve c) rest =
        (
            [PathPoint al, lc] <> nextLeft,
            nextRight <> [rc, PathPoint ar]
        )
      where
        apos = curvePosition c 0
        bpos = curvePosition c 1
        adir = curveDirection c 0
        bdir = curveDirection c 1
        (lc, rc) = leftRightCurve thickness c
        (al, ar) = leftRightDir2 thickness apos prevDir adir
        (nextLeft, nextRight) = onPoint bpos bdir rest


    headPos = pathPartStart p
    lastPos = pathPartEnd (last ps)
    lastDir = if closed
        then Just $ signorm (headPos - lastPos)
        else Nothing

    (lres, rres) = case lastDir of
        Just ld -> onIter ld p (q : ps)
        Nothing -> let
                headDir = signorm (pathPartStart q - pathPartEnd p)
            in case p of
                PathPoint a -> ([PathPoint lp] <> nl, nr <> [PathPoint rp])
                  where
                    (lp, rp) = leftRightDir thickness a headDir
                    (nl, nr) = onIter headDir q ps
                PathCurve c -> ([lc] <> nl, nr <> [rc])
                  where
                    bpos = curvePosition c 1
                    bdir = curveDirection c 1
                    (lc, rc) = leftRightCurve thickness c
                    (nl, nr) = onPoint bpos bdir (q: ps)



leftRightDir :: Float -> V2 Float -> V2 Float -> (V2 Float, V2 Float)
leftRightDir thickness p d = (p + h, p - h)
  where
    h = perp d ^* (thickness * 0.5)

leftRightDir2 :: Float -> V2 Float -> V2 Float -> V2 Float -> (V2 Float, V2 Float)
leftRightDir2 thickness p d1 d2 = case l of
    Just left -> (left, p + p - left)
    Nothing -> leftRightDir thickness p d1
  where
    e1 = perp d1 ^* (thickness * 0.5)
    e2 = perp d2 ^* (thickness * 0.5)
    l1 = p + e1
    l2 = p + e2
    l = lineIntersect l1 d1 l2 d2

leftRightCurve :: Float -> Curve -> (PathPart, PathPart)
leftRightCurve thickness curve = (
        PathCurve $ half `alongsideOf` curve,
        PathCurve $ half `alongsideOf` Curve.reverse curve
    )
  where
    half = thickness * 0.5

