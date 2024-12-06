module Graphics.Pizza.Internal.Geometry where

import Linear

{-| 
  Get factor for intersecting point, defined by the lines.

  When (V2 x y) = lineIntersectFactor ap ad bp bd, 
  The intersect point is (ap + ad * x) == (bp + bd * y).
-}
lineIntersectFactor :: V2 Float -> V2 Float -> V2 Float -> V2 Float -> Maybe (V2 Float)
lineIntersectFactor ap ad bp bd
  | det22 dirMat == 0 = Nothing
  | otherwise         = Just ((bp - ap) *! inv22 dirMat)
  where
    dirMat = V2 ad bd

{-|
  Get the intersecting point.
-}
lineIntersect :: V2 Float -> V2 Float -> V2 Float -> V2 Float -> Maybe (V2 Float)
lineIntersect ap ad bp bd 
  | det22 dirMat == 0 = Nothing
  | otherwise         = Just (ap + (ad ^* x))
  where
    dirMat = V2 ad bd
    V2 x _ = (bp - ap) *! inv22 (V2 ad bd)