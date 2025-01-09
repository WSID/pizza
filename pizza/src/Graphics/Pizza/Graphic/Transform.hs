module Graphics.Pizza.Graphic.Transform where

import Linear

import Graphics.Pizza.Internal.Geometry (rotMat)

data Transform = Transform {
    transMatrix :: M22 Float,
    transTrans :: V2 Float
} deriving (Show, Eq)

instance Semigroup Transform where
    Transform am at <> Transform bm bt = Transform (bm !*! am) (bm !* at + bt)

instance Monoid Transform where
    mempty = Transform identity zero

instance Transformable Transform where
    transform a b = b <> a
    translate v (Transform m t) = Transform m (t + v)
    rotate r (Transform m t) = Transform (rm !*! m) (rm !* t)
      where rm = rotMat r
    scale s (Transform m t) = Transform (fmap (* s) m) (t * s)

runTransform :: Transform -> V2 Float -> V2 Float
runTransform (Transform m t) v = (m !* v) + t

fromTranslate :: V2 Float -> Transform
fromTranslate = Transform identity

fromRotation :: Float -> Transform
fromRotation ang = Transform (rotMat ang) zero

fromScale :: V2 Float -> Transform
fromScale (V2 x y) = Transform (V2 (V2 x 0) (V2 0 y)) zero

fromPose :: V2 Float -> Float -> V2 Float -> Transform
fromPose pos rot scl = Transform (V2 (scl * r0) (scl * r1)) pos
  where
    V2 r0 r1 = rotMat rot


{-|
A transformable items.
-}
class Transformable a where
    {-# MINIMAL transform #-}
    transform :: Transform -> a -> a

    translate :: V2 Float -> a -> a
    translate trs = transform (fromTranslate trs)

    rotate :: Float -> a -> a
    rotate rot = transform (fromRotation rot)

    scale :: V2 Float -> a -> a
    scale scl = transform (fromScale scl)

