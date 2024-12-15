module Graphics.Pizza.Graphic.Stroke where

import Linear
import Graphics.Pizza.Graphic.Curve as Curve
import Graphics.Pizza.Graphic.Path
import Graphics.Pizza.Internal.Geometry


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

