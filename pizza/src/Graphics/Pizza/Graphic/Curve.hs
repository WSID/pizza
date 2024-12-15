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

fromVPoly :: VPoly -> Curve
fromVPoly poly = Curve {
    curvePosition = runVPoly poly,
    curveDirection = runVPoly dp
}
  where
    dp = derivVPoly poly


