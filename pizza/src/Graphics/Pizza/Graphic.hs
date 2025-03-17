{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Graphics.Pizza.Graphic (
    module Graphics.Pizza.Graphic.Curve,
    module Graphics.Pizza.Graphic.Dash,
    module Graphics.Pizza.Graphic.Path,
    module Graphics.Pizza.Graphic.Stroke,
    module Graphics.Pizza.Graphic.Transform,

    Blend ( .. ),
    DrawAttributes ( .. ),
    DrawItem ( .. ),
    Graphics ( .. ),
    Pattern ( .. ),

    applyOpacity,

    dashStroke
) where

-- linear
import Linear

import Graphics.Pizza.Device.Image
import Graphics.Pizza.Graphic.Curve
import Graphics.Pizza.Graphic.Dash
import Graphics.Pizza.Graphic.Stroke
import Graphics.Pizza.Graphic.Path
import Graphics.Pizza.Graphic.Transform

data Blend =
    BlendNormal |
    BlendMultiply |
    BlendScreen |
    BlendOverlay |
    BlendDarken |
    BlendLighten |
    BlendColorDodge |
    BlendColorBurn |
    BlendHardLight |
    BlendSoftLight |
    BlendDifference |
    BlendExclusion |
    BlendHue |
    BlendSaturation |
    BlendColor |
    BlendLuminosity

data DrawAttributes = DrawAttributes {
    drawPattern :: Pattern,
    drawTransform :: Transform,
    drawBlend :: Blend,
    drawOpacity :: Float
}

instance Transformable DrawAttributes where 
    transform tr attr = attr { drawTransform = transform tr $ drawTransform attr }

data DrawItem = DrawShape [Path] DrawAttributes

instance Transformable DrawItem where
    transform tr (DrawShape paths attr) = DrawShape paths (transform tr attr)

newtype Graphics = Graphics [DrawItem]

instance Transformable Graphics where
    transform tr (Graphics items) = Graphics (fmap (transform tr) items)

instance Semigroup Graphics where
    Graphics a <> Graphics b = Graphics (a <> b)

instance Monoid Graphics where
    mempty = Graphics mempty

data Pattern =
    PatternSolid (V4 Float) |
    PatternLinear (V2 Float) (V2 Float) (V4 Float) (V4 Float) |
    PatternRadial (V2 Float) Float (V4 Float) (V4 Float) |
    forall px. PatternImage (Image px) Transform Float

applyOpacityV :: Float -> V4 Float -> V4 Float
applyOpacityV opacity (V4 r g b a) = V4 r g b (a * opacity)

applyOpacity :: Float -> Pattern -> Pattern
applyOpacity opacity (PatternSolid c) = PatternSolid (applyOpacityV opacity c)
applyOpacity opacity (PatternLinear start end sc ec) = PatternLinear start end (applyOpacityV opacity sc) (applyOpacityV opacity ec)
applyOpacity opacity (PatternRadial center radius sc ec) = PatternRadial center radius (applyOpacityV opacity sc) (applyOpacityV opacity ec)
applyOpacity opacity (PatternImage image trans o) = PatternImage image trans (opacity * o)


dashStroke :: DashPattern -> StrokeOption -> StrokeEnd -> Path -> [Path]
dashStroke pat option StrokeClose path = case dash True pat path of
    DashClose p -> stroke option StrokeClose p
    Dash ps -> stroke option strokeEndNone =<< ps
dashStroke pat option (StrokeEnd scap ecap) path = case dash False pat path of
    DashClose _ -> error "Cannot happens!"
    Dash [] -> []
    Dash [p] -> stroke option (StrokeEnd scap ecap) p
    Dash (p: ps) ->
        stroke option (StrokeEnd scap strokeCapNone) p <>
        foldMap (stroke option strokeEndNone) (init ps) <>
        stroke option (StrokeEnd strokeCapNone ecap) (last ps)
