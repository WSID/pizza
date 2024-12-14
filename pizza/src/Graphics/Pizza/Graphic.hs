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

data StrokeJoinPoint = StrokeJoinPoint {
    strokeJoinPosition :: V2 Float,
    strokeJoinDirPrev :: V2 Float,
    strokeJoinDirNext :: V2 Float
}

data StrokeOption = StrokeOption {
    strokeThickness :: Float,
    strokeJoin :: Float -> StrokeJoinPoint -> (Path, Path)
}

type StrokeJoin = Float -> StrokeJoinPoint -> (Path, Path)

strokeJoinMiter :: Float -> StrokeJoinPoint -> (Path, Path)
strokeJoinMiter thickness (StrokeJoinPoint pos adir bdir) = (
        Path [PathPoint lp],
        Path [PathPoint rp]
    )
  where
    (lp, rp) = leftRightDir2 thickness pos adir bdir

strokeJoinRound :: Float -> StrokeJoinPoint -> (Path, Path)
strokeJoinRound thickness (StrokeJoinPoint pos adir bdir)
  | turnRight = (
            Path [
                arc pos (thickness * 0.5)
                    (aang + pi * 0.5)
                    (bangActual + pi * 0.5)
            ],
            Path [PathPoint rp]
        )
  | otherwise = (
            Path [PathPoint lp],
            Path [
                arc pos (thickness * 0.5)
                    (bangActual - pi * 0.5)
                    (aang - pi * 0.5)
            ]
        )
  where
    (lp, rp) = leftRightDir2 thickness pos adir bdir
    aang = unangle adir
    bang = unangle bdir
    bangActual
        | bang - aang > pi = bang - (2 * pi)
        | bang - aang < (-pi) = bang + (2 * pi)
        | otherwise = bang

    turnRight = adir `crossZ` bdir < 0

strokeJoinBevel :: Float -> StrokeJoinPoint -> (Path, Path)
strokeJoinBevel thickness (StrokeJoinPoint pos adir bdir)
  | turnRight = (
            Path [PathPoint la, PathPoint lb],
            Path [PathPoint rp]
        )
  | otherwise = (
            Path [PathPoint lp],
            Path [PathPoint rb, PathPoint ra]
        )
  where
    (lp, rp) = leftRightDir2 thickness pos adir bdir
    (la, ra) = leftRightDir thickness pos adir
    (lb, rb) = leftRightDir thickness pos bdir
    turnRight = adir `crossZ` bdir < 0

pathPartStart :: PathPart -> V2 Float
pathPartStart (PathPoint p) = p
pathPartStart (PathCurve (Curve pos _)) = pos 0

pathPartEnd :: PathPart -> V2 Float
pathPartEnd (PathPoint p) = p
pathPartEnd (PathCurve (Curve pos _)) = pos 1

stroke :: StrokeOption -> Bool -> Path -> [Path]
stroke _ _ (Path []) = []
stroke _ _ (Path [PathPoint _]) = []

stroke option closed (Path [PathCurve curve])
  | closed      = [ Path [lc], Path [rc] ]
  | otherwise   = [ Path [lc, rc] ]
  where
    (lc, rc) = leftRightCurve (strokeThickness option) curve

stroke option closed (Path (p: q: ps))
  | closed      = [Path lres, Path rres]
  | otherwise   = [Path (lres <> rres)]
  where
    thickness = strokeThickness option

    onPoint pos dir [] = (ljoin, rjoin)
      where
        (ljoin, rjoin) = case lastDir of
            Nothing -> let (lp, rp) = leftRightDir thickness pos dir in ([PathPoint lp], [PathPoint rp])
            Just ld -> let (Path lp, Path rp) = strokeJoin option thickness (StrokeJoinPoint pos dir ld) in (lp, rp)

    onPoint pos dir (cp: cps) = (ljoin <> nextLeft, nextRight <> rjoin)
      where
        nextPos = pathPartStart cp
        nextDir = signorm (nextPos - pos)
        (Path ljoin, Path rjoin) = strokeJoin option thickness (StrokeJoinPoint pos dir nextDir)
        (nextLeft, nextRight) = onIter nextDir cp cps

    onIter prevDir (PathPoint a) rest = onPoint a prevDir rest
    onIter prevDir (PathCurve c) rest =
        (
            aljoin <> [lc] <> nextLeft,
            nextRight <> [rc] <> arjoin
        )
      where
        apos = curvePosition c 0
        bpos = curvePosition c 1
        adir = curveDirection c 0
        bdir = curveDirection c 1
        (lc, rc) = leftRightCurve thickness c
        (Path aljoin, Path arjoin) = strokeJoin option thickness (StrokeJoinPoint apos prevDir adir)
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

