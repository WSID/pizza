module Graphics.Pizza.Graphic.Curve where

import Linear

import Graphics.Pizza.Internal.Util

-- | A curve on spatial.
data Curve = Curve {
    curvePosition :: Float -> V2 Float,
    curveDirection :: Float -> V2 Float
}

reverse :: Curve -> Curve
reverse curve = Curve {
    curvePosition = \t -> curvePosition curve (1 - t),
    curveDirection = \t -> - curveDirection curve (1 - t)
}

alongsideOf :: Float -> Curve -> Curve
alongsideOf amount curve = Curve {
    curvePosition = \t ->
        curvePosition curve t +
        (signorm . perp $ curveDirection curve t) ^* amount,

    -- TODO: Direction is incorrect!
    curveDirection = curveDirection curve
}

subCurve :: Float -> Float -> Curve -> Curve
subCurve start end (Curve pos dir) = Curve (pos . trans) (dir . trans)
  where
    len = end - start
    trans t = start + len * t


fromVPoly :: VPoly -> Curve
fromVPoly poly = Curve {
    curvePosition = runVPoly poly,
    curveDirection = runVPoly dp
}
  where
    dp = derivVPoly poly

data CurveRunnerEntry = CurveRunnerEntry {
    curveRunnerEntryDistance :: Float,
    curveRunnerEntryStart :: Float,
    curveRunnerEntryEnd :: Float
}

curveRunnerEntryAt :: CurveRunnerEntry -> Float -> Float
curveRunnerEntryAt (CurveRunnerEntry d s e) x = s + (e - s) * (x / d)

data CurveRunner = CurveRunner {
    curveRunnerEntries :: [CurveRunnerEntry],
    curveRunnerLeftover :: Float
}

mkCurveRunner :: Curve -> Int -> CurveRunner
mkCurveRunner curve n
  | n <= 0 = CurveRunner [] 0
  | otherwise = CurveRunner (zipWith3 CurveRunnerEntry dists ts (tail ts)) 0
  where
    ts = fmap (\i -> fromIntegral i / fromIntegral n) [0 .. n]
    poss = fmap (curvePosition curve) ts
    dists = zipWith distance poss (tail poss)

data CurveRunResult = CurveRunning Float CurveRunner | CurveDone Float

runCurveRunner :: CurveRunner -> Float -> CurveRunResult

runCurveRunner (CurveRunner [] _) l = CurveDone l

runCurveRunner (CurveRunner (e: es) leftover) l
  | nextLeftover > entryDist = runCurveRunner (CurveRunner es 0) (nextLeftover - entryDist)
  | otherwise = CurveRunning (curveRunnerEntryAt e nextLeftover) (CurveRunner (e: es) nextLeftover)
  where
    entryDist = curveRunnerEntryDistance e
    nextLeftover = l + leftover

