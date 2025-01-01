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

-- Curves

bezier :: V2 Float -> [V2 Float] -> V2 Float -> Curve
bezier start controls end = fromVPoly $ bezierPoly start controls end

arc :: V2 Float -> Float -> Float -> Float -> Curve
arc center radius start end = Curve {
    curvePosition = \t -> center + angle (start + (end - start) * t) ^* radius,
    curveDirection = \t -> perp $ angle (start + (end - start) * t)
}

-- Curve Runner

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
  | otherwise = CurveRunner (zipWith3 CurveRunnerEntry dists inputs (tail inputs)) 0
  where
    inputs = fmap (\i -> fromIntegral i / fromIntegral n) [0 .. n]
    poss = fmap (curvePosition curve) inputs
    dists = zipWith distance poss (tail poss)

data CurveRunResult = CurveRunning Float CurveRunner | CurveDone Float

runCurveRunner :: CurveRunner -> Float -> CurveRunResult

runCurveRunner (CurveRunner [] _) len = CurveDone len

runCurveRunner (CurveRunner (entry: es) entryLeft) len
  | nextEntryLeft > entryDist =
        runCurveRunner
            (CurveRunner es 0)
            (nextEntryLeft - entryDist)
  | otherwise =
        CurveRunning
            (curveRunnerEntryAt entry nextEntryLeft)
            (CurveRunner (entry: es) nextEntryLeft)
  where
    entryDist = curveRunnerEntryDistance entry
    nextEntryLeft = len + entryLeft


-- Utility for curves

bezierPoly :: V2 Float -> [V2 Float] -> V2 Float -> VPoly
bezierPoly start [] end = VPoly [start, end - start]
bezierPoly start controls end = result
    where
        a = bezierPoly start (init controls) (last controls)
        b = bezierPoly (head controls) (tail controls) end
        VPoly bf = subVPoly b a
        result = addVPoly a (VPoly (zero : bf))
