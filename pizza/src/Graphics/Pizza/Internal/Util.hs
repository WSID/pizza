module Graphics.Pizza.Internal.Util where

import Linear


{- Dense Vector Polynomial for scalar input
 -}
newtype VPoly = VPoly { factors :: [V2 Float] }

runVPoly :: Float -> VPoly -> V2 Float
runVPoly x (VPoly f) = sum $ zipWith (^*) f $ iterate (* x) 1


addVPoly :: VPoly -> VPoly -> VPoly
addVPoly (VPoly a) (VPoly b) = VPoly $ go a b
  where
    go [] bs = bs
    go as [] = as
    go (ah: as) (bh: bs) = ah + bh : go as bs

negVPoly :: VPoly -> VPoly
negVPoly (VPoly f) = VPoly (fmap negate f)

subVPoly :: VPoly -> VPoly -> VPoly
subVPoly a b = addVPoly a (negVPoly b)

smulVPoly :: Float -> VPoly -> VPoly
smulVPoly a (VPoly b) = VPoly $ fmap (a *^) b

lerpVPoly :: Float -> VPoly -> VPoly -> VPoly
lerpVPoly a b c = addVPoly (smulVPoly (1 - a) b) (smulVPoly a c)

-- | Get Derivative polynomial.
--
-- Applying the rule of `x^n => n * x^(n - 1)`
--
-- Constant part is dropped
derivVPoly :: VPoly -> VPoly
derivVPoly (VPoly []) = VPoly []
derivVPoly (VPoly (_: fs)) = VPoly $ zipWith (*^) [1 ..] fs 
