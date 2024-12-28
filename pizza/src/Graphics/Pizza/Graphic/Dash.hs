module Graphics.Pizza.Graphic.Dash where

import Linear

import Graphics.Pizza.Graphic.Curve
import Graphics.Pizza.Graphic.Path


data DashPattern = DashPattern Bool [Float] deriving (Eq, Show)

dashPatternNone :: DashPattern
dashPatternNone = DashPattern False []

dashPatternFull :: DashPattern
dashPatternFull = DashPattern True []

dashPatternStartOn :: DashPattern -> Bool
dashPatternStartOn (DashPattern on _) = on

dashPatternEndOn :: DashPattern -> Bool
dashPatternEndOn (DashPattern on []) = on
dashPatternEndOn (DashPattern on [_]) = not on
dashPatternEndOn (DashPattern on (_: _: ps)) = dashPatternEndOn (DashPattern on ps)

dashPatternCons :: DashPattern -> (Bool, Maybe (Float, DashPattern))
dashPatternCons (DashPattern on []) = (on, Nothing)
dashPatternCons (DashPattern on (p: ps)) = (on, Just (p, DashPattern (not on) ps))

data DashState = DashState {
    dashStateOn :: Maybe [PathPart],
    dashStatePattern :: [Float],
    dashStateAccum :: [[PathPart]]
}

dashBetween :: V2 Float -> V2 Float -> DashState -> DashState
dashBetween start end = go 0
  where
    segDisp = end - start
    segDist = norm segDisp
    segDir = segDisp ^/ segDist

    go :: Float -> DashState -> DashState
    go l s = case dashStatePattern s of
        [] -> s
        (pat: ps) -> case dashStateOn s of
            Nothing
              | nl < segDist -> go nl $ DashState (Just [np]) ps (dashStateAccum s)
              | otherwise    -> DashState Nothing (nl - segDist: ps) (dashStateAccum s)
            Just wip
              | nl < segDist -> go nl $ DashState Nothing ps (dashStateAccum s <> [wip <> [np]])
              | otherwise    -> DashState (Just wip) (nl - segDist: ps) (dashStateAccum s)
          where
            nl = l + pat
            np = PathPoint (start + nl *^ segDir)

dashPoint :: V2 Float -> DashState -> DashState
dashPoint pos s = s { dashStateOn = (<> [PathPoint pos]) <$> dashStateOn s }

dashCurve :: Curve -> DashState -> DashState
dashCurve curve state = go 0 (mkCurveRunner curve 32) state
  where
    go :: Float -> CurveRunner -> DashState -> DashState
    go ts r s = case dashStatePattern s of
      [] -> case dashStateOn s of
        Nothing -> s
        Just wip -> DashState (Just (wip <> [PathCurve (subCurve ts 1 curve)])) [] (dashStateAccum s)

      (pat: ps) -> case runCurveRunner r pat of
          CurveRunning t nr -> case dashStateOn s of
              Nothing -> go t nr (DashState (Just []) ps (dashStateAccum s))
              Just wip -> go t nr (DashState Nothing ps (dashStateAccum s <> [wip <> [PathCurve (subCurve ts t curve)]]))
          CurveDone rl -> case dashStateOn s of
              Nothing -> s {dashStatePattern = rl: ps}
              Just wip -> DashState (Just (wip <> [PathCurve (subCurve ts 1 curve)])) (rl: ps) (dashStateAccum s)

dash :: DashPattern -> Path -> [Path]
dash _ (Path []) = []
dash (DashPattern on pat) (Path ps) = Path <$> maybe accum (\o -> accum <> [o]) lon
  where
    go [] s = s
    go [PathPoint p] s = dashPoint p s
    go [PathCurve c] s = dashCurve c s
    go (PathPoint p: q: r) s = go (q: r) $ dashBetween p (pathPartStart q) $ dashPoint p s
    go (PathCurve c: q: r) s = go (q: r) $ dashBetween (curvePosition c 1) (pathPartStart q) $ dashCurve c s
    DashState lon _ accum = go ps (DashState (if on then Just [] else Nothing) pat [])

