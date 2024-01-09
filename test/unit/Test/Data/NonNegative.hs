module Test.Data.NonNegative (props) where

import Data.Text.Display qualified as D
import Hedgehog (MonadGen, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import Numeric.Algebra.Additive.ASemigroup (ASemigroup ((.+.)))
import Numeric.Algebra.Multiplicative.MGroup (MGroup ((.%.)))
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup ((.*.)))
import Numeric.Data.NonNegative qualified as NonNeg
import Numeric.Data.NonNegative.Internal
  ( NonNegative
      ( MkNonNegative,
        UnsafeNonNegative
      ),
  )
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.HUnit ((@=?))
import Test.Tasty.HUnit qualified as HUnit
import Test.TestBounds (TestBounds (maxVal, minVal))
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Numeric.Data.NonNegative"
    [ mkNonNegativeSucceeds,
      mkNonNegativeFails,
      testUnsafe,
      addTotal,
      multTotal,
      divTotal,
      showSpecs,
      displaySpecs
    ]

mkNonNegativeSucceeds :: TestTree
mkNonNegativeSucceeds =
  Utils.testPropertyCompat "x >= 0 succeeds" "mkNonNegativeSucceeds" $
    H.property $ do
      x <- H.forAll nonneg
      Just (NonNeg.reallyUnsafeNonNegative x) === NonNeg.mkNonNegative x

mkNonNegativeFails :: TestTree
mkNonNegativeFails =
  Utils.testPropertyCompat "x < 0 fails" "mkNonNegativeFails" $
    H.property $ do
      x <- H.forAll neg
      Nothing === NonNeg.mkNonNegative x

testUnsafe :: TestTree
testUnsafe = HUnit.testCase "Test unsafeNonNegative" $ do
  UnsafeNonNegative 5 @=? NonNeg.unsafeNonNegative @Integer 5

  Utils.assertPureErrorCall expectedEx (NonNeg.unsafeNonNegative @Integer (-1))
  where
    expectedEx = "Numeric.Data.NonNegative.unsafeNonNegative: Received value < zero: -1"

addTotal :: TestTree
addTotal =
  Utils.testPropertyCompat "(.+.) is total" "addTotal" $
    H.property $ do
      px@(MkNonNegative x) <- H.forAll nonnegative
      py@(MkNonNegative y) <- H.forAll nonnegative
      let MkNonNegative pz = px .+. py
      x + y === pz

multTotal :: TestTree
multTotal =
  Utils.testPropertyCompat "(.*.) is total" "multTotal" $
    H.property $ do
      px@(MkNonNegative x) <- H.forAll nonnegative
      py@(MkNonNegative y) <- H.forAll nonnegative
      let MkNonNegative pz = px .*. py
      x * y === pz

divTotal :: TestTree
divTotal =
  Utils.testPropertyCompat "(.%.) is total" "divTotal" $
    H.property $ do
      px@(MkNonNegative x) <- H.forAll nonnegative
      py@(MkNonNegative y) <- H.forAll nonnegativeNZ
      let MkNonNegative pz = px .%. py
      x `div` y === pz

nonneg :: (MonadGen m) => m Int
nonneg = HG.integral $ HR.exponentialFrom 0 0 maxVal

neg :: (MonadGen m) => m Int
neg = HG.integral $ HR.exponentialFrom minVal -1 -1

nonnegative :: (MonadGen m) => m (NonNegative Int)
nonnegative = NonNeg.unsafeNonNegative <$> nonneg

nonnegativeNZ :: (MonadGen m) => m (NonNegative Int)
nonnegativeNZ = NonNeg.unsafeNonNegative <$> HG.integral (HR.exponentialFrom 1 1 maxVal)

showSpecs :: TestTree
showSpecs = HUnit.testCase "Shows NonNegative" $ do
  "UnsafeNonNegative 2" @=? show (NonNeg.unsafeNonNegative @Int 2)

displaySpecs :: TestTree
displaySpecs = HUnit.testCase "Displays NonNegative" $ do
  "2" @=? D.display (NonNeg.unsafeNonNegative @Int 2)
