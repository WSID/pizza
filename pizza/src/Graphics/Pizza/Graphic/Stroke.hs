module Graphics.Pizza.Graphic.Stroke where

import Linear
import Graphics.Pizza.Graphic.Curve as Curve
import Graphics.Pizza.Graphic.Path
import Graphics.Pizza.Internal.Geometry



data LeftRight = LeftRight [PathPart] [PathPart]

instance Semigroup LeftRight where
    LeftRight al ar <> LeftRight bl br = LeftRight (al <> bl) (br <> ar)

instance Monoid LeftRight where
    mempty = LeftRight [] []



data StrokeJoinPoint = StrokeJoinPoint {
    strokeJoinPosition :: V2 Float,
    strokeJoinDirPrev :: V2 Float,
    strokeJoinDirNext :: V2 Float
}

type StrokeJoin = Float -> StrokeJoinPoint -> LeftRight

strokeJoinMiter :: Float -> StrokeJoinPoint -> LeftRight
strokeJoinMiter thickness (StrokeJoinPoint pos adir bdir) = leftRightDir2 thickness pos adir bdir

strokeJoinRound :: Float -> StrokeJoinPoint -> LeftRight
strokeJoinRound thickness (StrokeJoinPoint pos adir bdir)
  | turnRight = LeftRight
            [
                arc pos (thickness * 0.5)
                    (aang + pi * 0.5)
                    (bangActual + pi * 0.5)
            ]
            rp
  | otherwise = LeftRight
            lp
            [
                arc pos (thickness * 0.5)
                    (bangActual - pi * 0.5)
                    (aang - pi * 0.5)
            ]
  where
    LeftRight lp  rp = leftRightDir2 thickness pos adir bdir
    aang = unangle adir
    bang = unangle bdir
    bangActual
        | bang - aang > pi = bang - (2 * pi)
        | bang - aang < (-pi) = bang + (2 * pi)
        | otherwise = bang

    turnRight = adir `crossZ` bdir < 0

strokeJoinBevel :: Float -> StrokeJoinPoint -> LeftRight
strokeJoinBevel thickness (StrokeJoinPoint pos adir bdir)
  | turnRight = LeftRight (la <> lb) rp
  | otherwise = LeftRight lp (rb <> ra)
  where
    LeftRight lp rp = leftRightDir2 thickness pos adir bdir
    LeftRight la ra = leftRightDir thickness pos adir
    LeftRight lb rb = leftRightDir thickness pos bdir
    turnRight = adir `crossZ` bdir < 0


data StrokeOption = StrokeOption {
    strokeThickness :: Float,
    strokeJoin :: Float -> StrokeJoinPoint -> LeftRight
}

defStrokeOption :: StrokeOption
defStrokeOption = StrokeOption {
    strokeThickness = 1.0,
    strokeJoin = strokeJoinMiter
}

stroke :: StrokeOption -> Bool -> Path -> [Path]
stroke _ _ (Path []) = []
stroke _ _ (Path [PathPoint _]) = []

stroke option closed (Path [PathCurve curve])
  | closed      = [ Path lc, Path rc ]
  | otherwise   = [ Path (lc <> rc) ]
  where
    LeftRight lc rc = leftRightCurve (strokeThickness option) curve

stroke option closed (Path (p: q: ps))
  | closed      = [Path lres, Path rres]
  | otherwise   = [Path (lres <> rres)]
  where
    headPos = pathPartStart p
    lastPos = pathPartEnd (last ps)
    lastDir = if closed
        then Just $ signorm (headPos - lastPos)
        else Nothing

    LeftRight lres rres = case lastDir of
        Just ld -> strokeIter option lastDir ld p (q: ps)
        Nothing -> strokeStart option p q ps


strokeStart :: StrokeOption -> PathPart -> PathPart -> [PathPart] -> LeftRight
strokeStart option (PathPoint pos) q ps =
    leftRightDir (strokeThickness option) pos nextDir <>
    strokeIter option Nothing nextDir q ps
  where
    next = pathPartStart q
    nextDir = signorm (next - pos)

strokeStart option (PathCurve curve) q ps =
    leftRightCurve (strokeThickness option) curve <>
    strokePoint option Nothing bdir bpos (q: ps)
  where
    bpos = curvePosition curve 1
    bdir = curveDirection curve 1

strokePoint :: StrokeOption -> Maybe (V2 Float) -> V2 Float -> V2 Float -> [PathPart] -> LeftRight
strokePoint option lastDir prevDir pos [] = case lastDir of
    Nothing -> leftRightDir thickness pos prevDir
    Just ld -> strokeJoin option thickness (StrokeJoinPoint pos prevDir ld)
  where
    thickness = strokeThickness option

strokePoint option lastDir prevDir pos (p: ps) =
    strokeJoin option thickness (StrokeJoinPoint pos prevDir nextDir) <>
    strokeIter option lastDir nextDir p ps
  where
    thickness = strokeThickness option
    nextPos = pathPartStart p
    nextDir = signorm (nextPos - pos)

strokeIter :: StrokeOption -> Maybe (V2 Float) -> V2 Float -> PathPart -> [PathPart] -> LeftRight
strokeIter option lastDir prevDir (PathPoint pos) rest = strokePoint option lastDir prevDir pos rest
strokeIter option lastDir prevDir (PathCurve curve) rest =
    strokeJoin option thickness (StrokeJoinPoint apos prevDir adir) <>
    leftRightCurve thickness curve <>
    strokePoint option lastDir bdir bpos rest
  where
    thickness = strokeThickness option
    apos = curvePosition curve 0
    bpos = curvePosition curve 1
    adir = curveDirection curve 0
    bdir = curveDirection curve 1


leftRightDir :: Float -> V2 Float -> V2 Float -> LeftRight
leftRightDir thickness p d = LeftRight [PathPoint (p + h)] [PathPoint (p - h)]
  where
    h = perp d ^* (thickness * 0.5)

leftRightDir2 :: Float -> V2 Float -> V2 Float -> V2 Float -> LeftRight
leftRightDir2 thickness p d1 d2 = case l of
    Just left -> LeftRight [PathPoint left] [PathPoint (p + p - left)]
    Nothing -> leftRightDir thickness p d1
  where
    e1 = perp d1 ^* (thickness * 0.5)
    e2 = perp d2 ^* (thickness * 0.5)
    l1 = p + e1
    l2 = p + e2
    l = lineIntersect l1 d1 l2 d2

leftRightCurve :: Float -> Curve -> LeftRight
leftRightCurve thickness curve = LeftRight
    [PathCurve $ half `alongsideOf` curve]
    [PathCurve $ half `alongsideOf` Curve.reverse curve]
  where
    half = thickness * 0.5



