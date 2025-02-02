module Graphics.Pizza.Painting where

import Linear
import Control.Arrow

import Graphics.Pizza.Graphic


data Pathing a = Pathing a Path

instance Functor Pathing where
    fmap f (Pathing a r) = Pathing (f a) r

instance Applicative Pathing where
    pure v = (Pathing v mempty)
    Pathing av ar <*> Pathing bv br = Pathing (av bv) (ar <> br)

instance Monad Pathing where
    Pathing av ar >>= f = let Pathing bv br = f av in Pathing bv (ar <> br)

pathingPoint :: V2 Float -> Pathing ()
pathingPoint v = Pathing () (Path [PathPoint v])

pathingCurve :: Curve -> Pathing ()
pathingCurve c = Pathing () (Path [PathCurve c])

listenPathing :: Pathing a -> Pathing (a, Path)
listenPathing (Pathing av ar) = Pathing (av, ar) ar

passPathing :: Pathing (a, Path -> Path) -> Pathing a
passPathing (Pathing (a, f) ar) = Pathing a (f ar)

listensPathing :: (Path -> b) -> Pathing a -> Pathing (a, b)
listensPathing f (Pathing av ar) = Pathing (av, f ar) ar

censorPathing :: (Path -> Path) -> Pathing a -> Pathing a
censorPathing f (Pathing av ar) = Pathing av (f ar)



data Shaping a = Shaping a [Path]

instance Functor Shaping where
    fmap f (Shaping a r) = Shaping (f a) r

instance Applicative Shaping where
    pure v = Shaping v mempty
    Shaping av ar <*> Shaping bv br = Shaping (av bv) (ar <> br)

instance Monad Shaping where
    Shaping av ar >>= f = let Shaping bv br = f av in Shaping bv (ar <> br)

shapingPath :: a -> Path -> Shaping a
shapingPath a p = Shaping a [p]

shapingPathing :: Pathing a -> Shaping a
shapingPathing (Pathing a r) = Shaping a [r]

listenShaping :: Shaping a -> Shaping (a, [Path])
listenShaping (Shaping av ar) = Shaping (av, ar) ar

passShaping :: Shaping (a, [Path] -> [Path]) -> Shaping a
passShaping (Shaping (a, f) ar) = Shaping a (f ar)

listensShaping :: ([Path] -> b) -> Shaping a -> Shaping (a, b)
listensShaping f (Shaping av ar) = Shaping (av, f ar) ar

censorShaping :: ([Path] -> [Path]) -> Shaping a -> Shaping a
censorShaping f (Shaping av ar) = Shaping av (f ar)



data PaintingEnv = PaintingEnv {
        paintingAttributes :: DrawAttributes,
        paintingThickness :: Float,
        paintingEndCaps :: (StrokeCap, StrokeCap),
        paintingJoin :: StrokeJoin,
        paintingDashPattern :: Maybe DashPattern
    }

defPaintingEnv :: PaintingEnv
defPaintingEnv = PaintingEnv {
        paintingAttributes = DrawAttributes {
            drawPattern = PatternSolid (V4 1 1 1 1),
            drawTransform = mempty,
            drawBlend = BlendNormal,
            drawOpacity = 1.0
        },
        paintingThickness = 1.0,
        paintingEndCaps = (strokeCapNone, strokeCapNone),
        paintingJoin = strokeJoinMiter,
        paintingDashPattern = Nothing
    }

newtype Painting a = Painting { runPainting :: PaintingEnv -> (a, Graphics) }

runPainting_ :: Painting x -> PaintingEnv -> Graphics
runPainting_ (Painting p) env = snd $ p env

instance Functor Painting where
    fmap f (Painting p) = Painting $ first f . p

instance Applicative Painting where
    pure v = Painting $ const (v, mempty)
    Painting a <*> Painting b = Painting $ \env ->
        let
            (av, ar) = a env
            (bv, br) = b env
        in (av bv, ar <> br)

instance Monad Painting where
    Painting a >>= f = Painting $ \env ->
        let
            (av, ar) = a env
            Painting b = f av
            (bv, br) = b env
        in (bv, ar <> br)


askPaintingEnv :: Painting PaintingEnv
askPaintingEnv = Painting $ \env -> (env, mempty)

asksPaintingEnv :: (PaintingEnv -> a) -> Painting a
asksPaintingEnv f = fmap f askPaintingEnv

asksPaintingAttribtutes :: (DrawAttributes -> a) -> Painting a
asksPaintingAttribtutes f = asksPaintingEnv (f . paintingAttributes)

askTransform :: Painting Transform
askTransform = asksPaintingAttribtutes drawTransform

askPattern :: Painting Pattern
askPattern = asksPaintingAttribtutes drawPattern

askBlend :: Painting Blend
askBlend = asksPaintingAttribtutes drawBlend

askOpacity :: Painting Float
askOpacity = asksPaintingAttribtutes drawOpacity

askThickness :: Painting Float
askThickness = asksPaintingEnv paintingThickness

askEndCaps :: Painting (StrokeCap, StrokeCap)
askEndCaps = asksPaintingEnv paintingEndCaps

askJoin :: Painting StrokeJoin
askJoin = asksPaintingEnv paintingJoin

askDashPattern :: Painting (Maybe DashPattern)
askDashPattern = asksPaintingEnv paintingDashPattern


localPaintingEnv :: (PaintingEnv -> PaintingEnv) -> Painting a -> Painting a
localPaintingEnv f (Painting p) = Painting (p . f)

localPaintingAttributes :: (DrawAttributes -> DrawAttributes) -> Painting a -> Painting a
localPaintingAttributes f = localPaintingEnv (\env -> env { paintingAttributes = f $ paintingAttributes env } ) 

localTransform :: (Transform -> Transform) -> Painting a -> Painting a
localTransform f = localPaintingAttributes (\attr -> attr { drawTransform = f $ drawTransform attr } )

localPattern :: (Pattern -> Pattern) -> Painting a -> Painting a
localPattern f = localPaintingAttributes (\attr -> attr { drawPattern = f $ drawPattern attr } )

localBlend :: (Blend -> Blend) -> Painting a -> Painting a
localBlend f = localPaintingAttributes (\attr -> attr { drawBlend = f $ drawBlend attr} )

localOpacity :: (Float -> Float) -> Painting a -> Painting a
localOpacity f = localPaintingAttributes (\attr -> attr { drawOpacity = f $ drawOpacity attr } )

localThickness :: (Float -> Float) -> Painting a -> Painting a
localThickness f = localPaintingEnv (\env -> env { paintingThickness = f $ paintingThickness env } )

localEndCaps :: ((StrokeCap, StrokeCap) -> (StrokeCap, StrokeCap)) -> Painting a -> Painting a
localEndCaps f = localPaintingEnv (\env -> env { paintingEndCaps = f $ paintingEndCaps env } )

localJoin :: (StrokeJoin -> StrokeJoin) -> Painting a -> Painting a
localJoin f = localPaintingEnv (\env -> env { paintingJoin = f $ paintingJoin env } )

localDashPattern :: (Maybe DashPattern -> Maybe DashPattern) -> Painting a -> Painting a
localDashPattern f = localPaintingEnv (\env -> env { paintingDashPattern = f $ paintingDashPattern env } )


painting :: (a, Graphics) -> Painting a
painting r = Painting $ const r

paint :: Graphics -> Painting ()
paint g = Painting $ const ((), g)

paintFill :: [Path] -> Painting ()
paintFill shape = Painting $ \env -> ((), Graphics [DrawShape shape (paintingAttributes env)])

paintStrokeOpen :: Path -> Painting ()
paintStrokeOpen path = Painting $ \env -> (
        (),
        Graphics [
            let
                option = StrokeOption (paintingThickness env) (paintingJoin env)
                ends = uncurry StrokeEnd (paintingEndCaps env)
                shape = case paintingDashPattern env of
                    Just dashPattern -> dashStroke dashPattern option ends path
                    Nothing -> stroke option ends path

            in DrawShape shape (paintingAttributes env)
        ]
    )

paintStrokeClose :: Path -> Painting ()
paintStrokeClose path = Painting $ \env -> (
        (),
        Graphics [
            let
                strokeOption = StrokeOption (paintingThickness env) (paintingJoin env)
                shape = case paintingDashPattern env of
                    Just dashPattern -> dashStroke dashPattern strokeOption StrokeClose path
                    Nothing -> stroke strokeOption StrokeClose path

            in DrawShape shape (paintingAttributes env)
        ]
    )


listenPainting :: Painting a -> Painting (a, Graphics)
listenPainting (Painting a) = Painting $ \env -> let (av, ar) = a env in ((av, ar), ar)

passPainting :: Painting (a, Graphics -> Graphics) -> Painting a
passPainting (Painting a) = Painting $ \env -> let ((av, f), ar) = a env in (av, f ar)

listensPainting :: (Graphics -> b) -> Painting a -> Painting (a, b)
listensPainting f (Painting a) = Painting $ \env -> let (av, ar) = a env in ((av, f ar), ar)

censorPainting :: (Graphics -> Graphics) -> Painting a -> Painting a
censorPainting f (Painting a) = Painting $ \env -> let (av, ar) = a env in (av, f ar)


paintFillShaping :: Shaping a -> Painting a
paintFillShaping (Shaping a r) = Painting $ \env -> (a, Graphics [DrawShape r (paintingAttributes env)])

paintStrokeOpenPathing :: Pathing a -> Painting a
paintStrokeOpenPathing (Pathing a r) = a <$ paintStrokeOpen r

paintStrokeClosePathing :: Pathing a -> Painting a
paintStrokeClosePathing (Pathing a r) = a <$ paintStrokeClose r
