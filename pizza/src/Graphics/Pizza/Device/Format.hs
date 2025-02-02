{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Graphics.Pizza.Device.Format where

import Data.Kind
import Data.Int
import Data.Word

import Foreign.Storable

import Linear

import qualified Vulkan as Vk


-- Newtypes for integers that treated floating on GPU.

-- | New type for unsigned normalized integer values.
--
-- CPU [0 .. maxBound] is mapped on GPU [0.0 .. 1.0]
newtype UNorm a = UNorm { getUNorm :: a } deriving (Eq, Ord, Enum, Bounded, Storable)

-- | New type for signed normalizaed integer values.
--
-- CPU [minBound .. maxBound] is mapped on GPU [-1.0 .. 1.0]
newtype SNorm a = SNorm { getSNorm :: a } deriving (Eq, Ord, Enum, Bounded, Storable)

-- | New type for unsigned scaled integer values.
--
-- CPU [0 .. maxBound] is mapped on GPU [0 .. fromIntegral maxBound]
newtype UScaled a = UScaled { getUScaled :: a } deriving (Eq, Ord, Enum, Bounded, Storable)

-- | New type for signed scaled integer values.
--
-- CPU [minBound .. maxBound] is mapped on GPU [fromIntegral minBound .. fromIntegral maxBound]
newtype SScaled a = SScaled { getSScaled :: a } deriving (Eq, Ord, Enum, Bounded, Storable)



-- Type Class Declaration

class Storable a => Format a where
    formatOf :: a -> Vk.Format

class Format a => FormatR a where
    {-# MINIMAL red, (setRed | modifyRed) #-}
    type Red a :: Type
    red :: a -> Red a

    setRed :: Red a -> a -> a
    setRed r = modifyRed (const r)

    modifyRed :: (Red a -> Red a) -> a -> a
    modifyRed f v = setRed (f $ red v) v

class Format a => FormatG a where
    {-# MINIMAL green, (setGreen | modifyGreen) #-}
    type Green a :: Type
    green :: a -> Green a

    setGreen :: Green a -> a -> a
    setGreen g = modifyGreen (const g)

    modifyGreen :: (Green a -> Green a) -> a -> a
    modifyGreen f v = setGreen (f $ green v) v

class Format a => FormatB a where
    {-# MINIMAL blue, (setBlue | modifyBlue) #-}
    type Blue a :: Type
    blue :: a -> Blue a

    setBlue :: Blue a -> a -> a
    setBlue b = modifyBlue (const b)

    modifyBlue :: (Blue a -> Blue a) -> a -> a
    modifyBlue f v = setBlue (f $ blue v) v

class Format a => FormatA a where
    {-# MINIMAL alpha, (setAlpha | modifyAlpha) #-}
    type Alpha a :: Type
    alpha :: a -> Alpha a

    setAlpha :: Alpha a -> a -> a
    setAlpha a = modifyAlpha (const a)

    modifyAlpha :: (Alpha a -> Alpha a) -> a -> a
    modifyAlpha f v = setAlpha (f $ alpha v) v

-- Common Instances

newtype VRGBA a = VRGBA { unVRGBA :: V4 a } deriving (Eq, Storable)

makeVRGBA :: a -> a -> a -> a -> VRGBA a
makeVRGBA r g b a = VRGBA (V4 r g b a)

instance Format (VRGBA a) => FormatR (VRGBA a) where
    type Red (VRGBA a) = a
    red (VRGBA (V4 r _ _ _)) = r
    setRed r (VRGBA (V4 _ g b a)) = VRGBA (V4 r g b a)
    modifyRed f (VRGBA (V4 r g b a)) = VRGBA (V4 (f r) g b a)

instance Format (VRGBA a) => FormatG (VRGBA a) where
    type Green (VRGBA a) = a
    green (VRGBA (V4 _ g _ _)) = g
    setGreen g (VRGBA (V4 r _ b a)) = VRGBA (V4 r g b a)
    modifyGreen f (VRGBA (V4 r g b a)) = VRGBA (V4 r (f g) b a)

instance Format (VRGBA a) => FormatB (VRGBA a) where
    type Blue (VRGBA a) = a
    blue (VRGBA (V4 _ _ b _)) = b
    setBlue b (VRGBA (V4 r g _ a)) = VRGBA (V4 r g b a)
    modifyBlue f (VRGBA (V4 r g b a)) = VRGBA (V4 r g (f b) a)

instance Format (VRGBA a) => FormatA (VRGBA a) where
    type Alpha (VRGBA a) = a
    alpha (VRGBA (V4 _ _ _ a)) = a
    setAlpha a (VRGBA (V4 r g b _)) = VRGBA (V4 r g b a)
    modifyAlpha f (VRGBA (V4 r g b a)) = VRGBA (V4 r g b (f a))

newtype VBGRA a = VBGRA { unVBGRA :: V4 a } deriving (Eq, Storable)

makeVBGRA :: a -> a -> a -> a -> VBGRA a
makeVBGRA r g b a = VBGRA (V4 r g b a)

instance Format (VBGRA a) => FormatR (VBGRA a) where
    type Red (VBGRA a) = a
    red (VBGRA (V4 _ _ r _)) = r
    setRed r (VBGRA (V4 b g _ a)) = VBGRA (V4 b g r a)
    modifyRed f (VBGRA (V4 b g r a)) = VBGRA (V4 b g (f r) a)

instance Format (VBGRA a) => FormatG (VBGRA a) where
    type Green (VBGRA a) = a
    green (VBGRA (V4 _ g _ _)) = g
    setGreen g (VBGRA (V4 b _ r a)) = VBGRA (V4 b g r a)
    modifyGreen f (VBGRA (V4 b g r a)) = VBGRA (V4 b (f g) r a)

instance Format (VBGRA a) => FormatB (VBGRA a) where
    type Blue (VBGRA a) = a
    blue (VBGRA (V4 b _ _ _)) = b
    setBlue b (VBGRA (V4 _ g r a)) = VBGRA (V4 b g r a)
    modifyBlue f (VBGRA (V4 b g r a)) = VBGRA (V4 (f b) g r a)

instance Format (VBGRA a) => FormatA (VBGRA a) where
    type Alpha (VBGRA a) = a
    alpha (VBGRA (V4 _ _ _ a)) = a
    setAlpha a (VBGRA (V4 b g r _)) = VBGRA (V4 b g r a)
    modifyAlpha f (VBGRA (V4 b g r a)) = VBGRA (V4 b g r (f a))


-- R8 G8 B8 A8
instance Format (VRGBA (UNorm Word8)) where
    formatOf _ = Vk.FORMAT_R8G8B8A8_UNORM

instance Format (VRGBA (SNorm Int8)) where
    formatOf _ = Vk.FORMAT_R8G8B8A8_SNORM

instance Format (VRGBA (UScaled Word8)) where
    formatOf _ = Vk.FORMAT_R8G8B8A8_USCALED

instance Format (VRGBA (SScaled Int8)) where
    formatOf _ = Vk.FORMAT_R8G8B8A8_SSCALED

instance Format (VRGBA Word8) where
    formatOf _ = Vk.FORMAT_R8G8B8A8_UINT

instance Format (VRGBA Int8) where
    formatOf _ = Vk.FORMAT_R8G8B8A8_UINT


-- B8 G8 R8 A8
instance Format (VBGRA (UNorm Word8)) where
    formatOf _ = Vk.FORMAT_B8G8R8A8_UNORM

instance Format (VBGRA (SNorm Int8)) where
    formatOf _ = Vk.FORMAT_B8G8R8A8_SNORM

instance Format (VBGRA (UScaled Word8)) where
    formatOf _ = Vk.FORMAT_B8G8R8A8_USCALED

instance Format (VBGRA (SScaled Int8)) where
    formatOf _ = Vk.FORMAT_B8G8R8A8_SSCALED

instance Format (VBGRA Word8) where
    formatOf _ = Vk.FORMAT_B8G8R8A8_UINT

instance Format (VBGRA Int8) where
    formatOf _ = Vk.FORMAT_B8G8R8A8_UINT

