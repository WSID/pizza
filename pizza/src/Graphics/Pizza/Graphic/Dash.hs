module Graphics.Pizza.Graphic.Dash where

import Data.Maybe

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
    dashStateOn :: Maybe Path,
    dashStatePattern :: [Float],
    dashStateAccum :: [Path]
}

dashBetween :: V2 Float -> V2 Float -> DashState -> DashState
dashBetween start end = go 0
  where
    segDisp = end - start
    segDist = norm segDisp
    segDir = segDisp ^/ segDist

    go :: Float -> DashState -> DashState
    go len state = case dashStatePattern state of
        [] -> state
        (pat: ps)
          | nextLen >= segDist -> state {
                    dashStatePattern = nextLen - segDist : ps
                }

          | otherwise -> go nextLen $ case dashStateOn state of
                Nothing -> state {
                        dashStateOn = Just newPath,
                        dashStatePattern = ps
                    }
                Just wip -> state {
                        dashStateOn = Nothing,
                        dashStatePattern = ps,
                        dashStateAccum = dashStateAccum state <> [wip <> newPath]
                    }

          where
            nextLen = len + pat
            newPath = Path [PathPoint (start + nextLen *^ segDir)]


dashPathPart :: PathPart -> DashState -> DashState
dashPathPart (PathPoint pos) state = state {
        dashStateOn = (<> Path [PathPoint pos]) <$> dashStateOn state
    }

dashPathPart (PathCurve curve) state = go 0 (mkCurveRunner curve 32) state
  where
    go :: Float -> CurveRunner -> DashState -> DashState
    go curveStart runner s = case dashStatePattern s of
      [] -> s {
            dashStateOn = (<> newPath 1) <$> dashStateOn s
        }

      (pat: ps) -> case runCurveRunner runner pat of
          CurveRunning curveEnd nextRunner -> go curveEnd nextRunner $ case dashStateOn s of
              Nothing -> s {
                    dashStateOn = Just mempty,
                    dashStatePattern = ps
                }
              Just wip -> s {
                    dashStateOn = Nothing,
                    dashStatePattern = ps,
                    dashStateAccum = dashStateAccum s <> [wip <> newPath curveEnd]
                }

          CurveDone remLen -> s {
                    dashStateOn = (<> newPath 1) <$> dashStateOn s,
                    dashStatePattern = remLen: ps
                }
      where
        newPath t = Path [ PathCurve (subCurve curveStart t curve) ]



data Dash = Dash [Path] | DashClose Path

dash :: Bool -> DashPattern -> Path -> Dash
dash _ _ (Path []) = Dash []
dash close (DashPattern on pat) path = case (close, finalOn) of

    -- When we dashing closed path, and we piled up some dash to the end.
    -- In case we dashed something at initial - we connect them.

    (True, Just wip) -> case finalAccum of
      [] -> DashClose wip
      (p: q) -> Dash (wip <> p : q)

    -- Other case, we just put on-going dash as last dash.

    _ -> Dash (finalAccum <> maybeToList finalOn)
  where
    initState = DashState {
            dashStateOn = if on then Just mempty else Nothing,
            dashStatePattern = pat,
            dashStateAccum = []
        }
    Path ps = path
    pathStart = pathPartStart $ head ps

    go (Path []) s = s

    go (Path [p]) s = if close
        then dashBetween (pathPartEnd p) pathStart $ dashPathPart p s
        else dashPathPart p s

    go (Path (p: q: r)) s =
        go (Path (q: r)) $
        dashBetween (pathPartEnd p) (pathPartStart q) $
        dashPathPart p s

    DashState finalOn _ finalAccum = go path initState

