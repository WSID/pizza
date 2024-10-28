module Graphics.Pizza.Graphic where

import Control.Applicative

-- linear
import Linear

data Graphics = Graphics Path Pattern

data Pattern =
    PatternSolid (V4 Float) |
    PatternLinear (V2 Float) (V2 Float) (V4 Float) (V4 Float) |
    PatternRadial (V2 Float) Float (V4 Float) (V4 Float)


data AABB = AABB !(V2 Float) !(V2 Float)

data Path = Path [V2 Float] Bool

-- Functions

pathBound :: Path -> Maybe AABB
pathBound (Path [] _) = Nothing
pathBound (Path v _) = Just $ AABB lt rb
  where
    lt = foldr1 (liftA2 min) v
    rb = foldr1 (liftA2 max) v

midPoint :: AABB -> V2 Float
midPoint (AABB lt rb) = lerp 0.5 lt rb
