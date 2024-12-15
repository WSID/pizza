module Graphics.Pizza.Graphic (
    module Graphics.Pizza.Graphic.Curve,
    module Graphics.Pizza.Graphic.Path,
    module Graphics.Pizza.Graphic.Stroke,
    Graphics ( .. ),
    Pattern ( .. )
) where

-- linear
import Linear

import Graphics.Pizza.Graphic.Curve
import Graphics.Pizza.Graphic.Stroke
import Graphics.Pizza.Graphic.Path

data Graphics = Graphics [Path] Pattern

data Pattern =
    PatternSolid (V4 Float) |
    PatternLinear (V2 Float) (V2 Float) (V4 Float) (V4 Float) |
    PatternRadial (V2 Float) Float (V4 Float) (V4 Float)


