module Test.Extra where

-- base
import Control.Monad

import GHC.Stack

-- HUnit
import Test.HUnit


defaultEpsilon :: Fractional a => a
defaultEpsilon = 0.0001

-- | Assert two values are near within user specified difference.
assertNearBy :: HasCallStack => (Fractional a, Ord a, Show a) => String -> a -> a -> a -> Assertion
assertNearBy message epsilon expected actual =
    when (abs (actual - expected) > epsilon) $ do
        assertFailure (message <> " Expected " <> show expected <> " but got " <> show actual)

assertNear :: HasCallStack => (Fractional a, Ord a, Show a) => String -> a -> a -> Assertion
assertNear message = assertNearBy message defaultEpsilon

(~~=@) :: HasCallStack => (Fractional a, Ord a, Show a) => a -> a -> Assertion
expected ~~=@ actual = assertNear "" expected actual

(~~=?) :: HasCallStack => (Fractional a, Ord a, Show a) => a -> a -> Test
expected ~~=? actual = TestCase $ expected ~~=@ actual


(~~@=) :: HasCallStack => (Fractional a, Ord a, Show a) => a -> a -> Assertion
actual ~~@= expected = assertNear "" expected actual

(~~?=) :: HasCallStack => (Fractional a, Ord a, Show a) => a -> a -> Test
actual ~~?= expected = TestCase $ actual ~~@= expected


