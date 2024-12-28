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

data Dash = Dash [Path] | DashClose Path

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

dashPathPart :: PathPart -> DashState -> DashState
dashPathPart (PathPoint p) = dashPoint p
dashPathPart (PathCurve c) = dashCurve c

dash :: Bool -> DashPattern -> Path -> Dash
dash _ _ (Path []) = Dash []
dash close (DashPattern on pat) (Path ps) = case (close, lon) of
    (True, Just wip) -> case accum of
      [] -> DashClose $ Path wip
      (p: q) -> Dash $ Path <$> (wip <> p : q)
    _ -> Dash $ Path <$> maybe accum (\o -> accum <> [o]) lon
  where
    pathStart = pathPartStart $ head ps
    go [] s = s
    go [p] s = if close
        then dashBetween (pathPartEnd p) pathStart $ dashPathPart p s
        else dashPathPart p s
    go (p: q: r) s = go (q: r) $ dashBetween (pathPartEnd p) (pathPartStart q) $ dashPathPart p s
    DashState lon _ accum = go ps (DashState (if on then Just [] else Nothing) pat [])
