module Graphics.Pizza.Graphic (
    module Graphics.Pizza.Graphic.Curve,
    module Graphics.Pizza.Graphic.Dash,
    module Graphics.Pizza.Graphic.Path,
    module Graphics.Pizza.Graphic.Stroke,
    module Graphics.Pizza.Graphic.Transform,

    DrawAttributes ( .. ),
    DrawItem ( .. ),
    Graphics ( .. ),
    Pattern ( .. ),

    dashStroke
) where

-- linear
import Linear

import Graphics.Pizza.Graphic.Curve
import Graphics.Pizza.Graphic.Dash
import Graphics.Pizza.Graphic.Stroke
import Graphics.Pizza.Graphic.Path
import Graphics.Pizza.Graphic.Transform

data DrawAttributes = DrawAttributes {
    drawPattern :: Pattern,
    drawTransform :: Transform
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
    PatternRadial (V2 Float) Float (V4 Float) (V4 Float)


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
