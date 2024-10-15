module Graphics.Pizza.Graphic where

-- linear
import Linear

data Pattern =
    PatternSolid (V4 Float) |
    PatternLinear (V2 Float) (V2 Float) (V4 Float) (V4 Float) |
    PatternRadial (V2 Float) Float (V4 Float) (V4 Float)
