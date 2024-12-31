module Graphics.Pizza.Graphic.Stroke where

import Data.Foldable
import Linear
import Graphics.Pizza.Graphic.Curve as Curve
import Graphics.Pizza.Graphic.Path
import Graphics.Pizza.Internal.Geometry


data LeftRight = LeftRight Path Path

instance Semigroup LeftRight where
    LeftRight al ar <> LeftRight bl br = LeftRight (al <> bl) (br <> ar)

instance Monoid LeftRight where
    mempty = LeftRight mempty mempty

data StrokeJoinPoint = StrokeJoinPoint {
    strokeJoinPosition :: V2 Float,
    strokeJoinDirPrev :: V2 Float,
    strokeJoinDirNext :: V2 Float
} deriving (Show)

type StrokeJoin = Float -> StrokeJoinPoint -> LeftRight

strokeJoinMiter :: Float -> StrokeJoinPoint -> LeftRight
strokeJoinMiter thickness (StrokeJoinPoint pos adir bdir) = leftRightDir2 thickness pos adir bdir

strokeJoinRound :: Float -> StrokeJoinPoint -> LeftRight
strokeJoinRound thickness (StrokeJoinPoint pos adir bdir)
  | turnRight = LeftRight
            (Path [
                arc pos (thickness * 0.5)
                    (aang + pi * 0.5)
                    (bangActual + pi * 0.5)
            ])
            rp
  | otherwise = LeftRight
            lp
            (Path [
                arc pos (thickness * 0.5)
                    (bangActual - pi * 0.5)
                    (aang - pi * 0.5)
            ])
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

type StrokeCap = Float -> V2 Float -> V2 Float -> Path

-- Empty cap: Do nothing
strokeCapNone :: Float -> V2 Float -> V2 Float -> Path
strokeCapNone _ _ _ = Path []

-- Round cap
strokeCapRound :: Float -> V2 Float -> V2 Float -> Path
strokeCapRound thickness pos dir = Path [ arc pos (thickness * 0.5) (ang - (0.5 * pi)) (ang + (0.5 * pi))]
  where
    ang = unangle dir

-- Square cap
strokeCapSquare :: Float -> V2 Float -> V2 Float -> Path
strokeCapSquare thickness pos dir = Path [
        PathPoint (pos + forward + right),
        PathPoint (pos + forward - right)
    ]
  where
    forward = dir ^* (0.5 * thickness)
    right = perp forward

data StrokeEnd = StrokeClose | StrokeEnd StrokeCap StrokeCap

strokeEndNone :: StrokeEnd
strokeEndNone = strokeEndBoth strokeCapNone

strokeEndBoth :: StrokeCap -> StrokeEnd
strokeEndBoth cap = StrokeEnd cap cap

stroke :: StrokeOption -> StrokeEnd -> Path -> [Path]
stroke _ _ (Path []) = []
stroke _ StrokeClose (Path [PathPoint _]) = []
stroke option (StrokeEnd startCap endCap) (Path [PathPoint pos]) = case result of
    [] -> []
    _ -> [Path result]
  where
    thickness = strokeThickness option
    Path startCapPart = startCap thickness pos (V2 (-1) 0)
    Path endCapPart = endCap thickness pos (V2 1 0)
    result = startCapPart <> endCapPart

stroke option StrokeClose (Path [PathCurve curve]) = [lc, rc]
  where
    LeftRight lc rc = leftRightCurve (strokeThickness option) curve

stroke option (StrokeEnd startCap endCap) (Path [PathCurve curve]) =
    [ startCapParts <> lc <> endCapParts <> rc ]
  where
    thickness = strokeThickness option
    LeftRight lc rc = leftRightCurve thickness curve
    apos = curvePosition curve 0
    adir = signorm $ curveDirection curve 0
    bpos = curvePosition curve 1
    bdir = signorm $ curveDirection curve 1
    startCapParts = startCap thickness apos (negate adir)
    endCapParts = endCap thickness bpos bdir

stroke option StrokeClose (Path (p: ps)) = [lres, rres]
  where
    psR = ps <> [p]
    dirs = zipWith (\a b -> signorm $ pathPartStart b - pathPartEnd a) (p: ps) psR
    dirsR = last dirs : init dirs
    LeftRight lres rres = fold $ zipWith3 (strokeIter option) (p: ps) dirsR dirs

stroke option (StrokeEnd sc ec) (Path (p : ps)) = [lres <> ecp <> rres <> scp]
  where
    thickness = strokeThickness option
    pl = last ps
    dirs = zipWith (\a b -> signorm $ pathPartStart b - pathPartEnd a) (p: ps) ps
    dirl = last dirs
    LeftRight lres rres =
        strokeStart option p (head dirs) <>
        fold (zipWith3 (strokeIter option) ps dirs (tail dirs)) <>
        strokeEnd option pl dirl

    apos = pathPartStart p
    bpos = pathPartEnd pl

    adir = case p of
        PathPoint _ -> head dirs
        PathCurve c -> signorm $ curveDirection c 0
    bdir = case pl of
        PathPoint _ -> dirl
        PathCurve c -> signorm $ curveDirection c 1

    scp = sc thickness apos (negate adir)
    ecp = ec thickness bpos bdir


strokeStart :: StrokeOption -> PathPart -> V2 Float -> LeftRight

strokeStart option (PathPoint pos) dir =
    leftRightDir (strokeThickness option) pos dir

strokeStart option (PathCurve curve) dir =
    leftRightCurve (strokeThickness option) curve <>
    strokeJoin option thickness (StrokeJoinPoint bpos bdir dir)
  where
    thickness = strokeThickness option
    bpos = curvePosition curve 1
    bdir = curveDirection curve 1


strokeIter :: StrokeOption -> PathPart -> V2 Float -> V2 Float -> LeftRight

strokeIter option (PathPoint pos) pdir ndir =
    strokeJoin option (strokeThickness option) (StrokeJoinPoint pos pdir ndir)

strokeIter option (PathCurve curve) pdir ndir =
    strokeJoin option thickness (StrokeJoinPoint apos pdir adir) <>
    leftRightCurve thickness curve <>
    strokeJoin option thickness (StrokeJoinPoint bpos bdir ndir)
  where
    thickness = strokeThickness option
    apos = curvePosition curve 0
    adir = curveDirection curve 0
    bpos = curvePosition curve 1
    bdir = curveDirection curve 1

strokeEnd :: StrokeOption -> PathPart -> V2 Float -> LeftRight
strokeEnd option (PathPoint pos) dir =
    leftRightDir (strokeThickness option) pos dir

strokeEnd option (PathCurve curve) dir =
    strokeJoin option thickness (StrokeJoinPoint apos adir dir) <>
    leftRightCurve (strokeThickness option) curve
  where
    thickness = strokeThickness option
    apos = curvePosition curve 0
    adir = curveDirection curve 0

leftRightDir :: Float -> V2 Float -> V2 Float -> LeftRight
leftRightDir thickness p d = LeftRight
    (Path [PathPoint (p + h)])
    (Path [PathPoint (p - h)])
  where
    h = perp d ^* (thickness * 0.5)

leftRightDir2 :: Float -> V2 Float -> V2 Float -> V2 Float -> LeftRight
leftRightDir2 thickness p d1 d2 = case l of
    Just left -> LeftRight
        (Path [PathPoint left])
        (Path [PathPoint (p + p - left)])
    Nothing -> leftRightDir thickness p d1
  where
    e1 = perp d1 ^* (thickness * 0.5)
    e2 = perp d2 ^* (thickness * 0.5)
    l1 = p + e1
    l2 = p + e2
    l = lineIntersect l1 d1 l2 d2

leftRightCurve :: Float -> Curve -> LeftRight
leftRightCurve thickness curve = LeftRight
    (Path [PathCurve $ half `alongsideOf` curve])
    (Path [PathCurve $ half `alongsideOf` Curve.reverse curve])
  where
    half = thickness * 0.5
