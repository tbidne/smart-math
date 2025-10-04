module Test.Data.NonNegative (props) where

import Data.Text.Display qualified as D
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
import Test.Prelude
import Test.TestBounds (TestBounds (maxVal, minVal))
import Utils qualified

props :: TestTree
props =
  testGroup
    "Numeric.Data.NonNegative"
    [ mkNonNegativeSucceeds,
      mkNonNegativeFails,
      testUnsafe,
      addTotal,
      multTotal,
      divTotal,
      elimProps,
      showSpecs,
      displaySpecs
    ]

mkNonNegativeSucceeds :: TestTree
mkNonNegativeSucceeds =
  testPropertyCompat "x >= 0 succeeds" "mkNonNegativeSucceeds" $
    property $ do
      x <- forAll nonneg
      Right (NonNeg.reallyUnsafeNonNegative x) === NonNeg.mkNonNegative x

mkNonNegativeFails :: TestTree
mkNonNegativeFails =
  testPropertyCompat "x < 0 fails" "mkNonNegativeFails" $
    property $ do
      x <- forAll neg
      assertLeftHH "Numeric.Data.NonNegative: Received value < zero:" (NonNeg.mkNonNegative x)

testUnsafe :: TestTree
testUnsafe = testCase "Test unsafeNonNegative" $ do
  UnsafeNonNegative 5 @=? NonNeg.unsafeNonNegative @Integer 5

  Utils.assertPureErrorCall expectedEx (NonNeg.unsafeNonNegative @Integer (-1))
  where
    expectedEx = "Numeric.Data.NonNegative: Received value < zero: -1"

addTotal :: TestTree
addTotal =
  testPropertyCompat "(.+.) is total" "addTotal" $
    property $ do
      px@(MkNonNegative x) <- forAll nonnegative
      py@(MkNonNegative y) <- forAll nonnegative
      let MkNonNegative pz = px .+. py
      x + y === pz

multTotal :: TestTree
multTotal =
  testPropertyCompat "(.*.) is total" "multTotal" $
    property $ do
      px@(MkNonNegative x) <- forAll nonnegative
      py@(MkNonNegative y) <- forAll nonnegative
      let MkNonNegative pz = px .*. py
      x * y === pz

divTotal :: TestTree
divTotal =
  testPropertyCompat "(.%.) is total" "divTotal" $
    property $ do
      px@(MkNonNegative x) <- forAll nonnegative
      py@(MkNonNegative y) <- forAll nonnegativeNZ
      let MkNonNegative pz = px .%. py
      x `div` y === pz

nonneg :: Gen Int
nonneg = HG.integral $ HR.exponentialFrom 0 0 maxVal

neg :: Gen Int
neg = HG.integral $ HR.exponentialFrom minVal -1 -1

nonnegative :: Gen (NonNegative Int)
nonnegative = NonNeg.unsafeNonNegative <$> nonneg

nonnegativeNZ :: Gen (NonNegative Int)
nonnegativeNZ = NonNeg.unsafeNonNegative <$> HG.integral (HR.exponentialFrom 1 1 maxVal)

elimProps :: TestTree
elimProps =
  testPropertyCompat desc "elimProps" $
    property $ do
      mp@(MkNonNegative n) <- forAll nonnegative

      n === NonNeg.unNonNegative mp
      n === mp.unNonNegative
      n === view #unNonNegative mp
      n === view NonNeg._MkNonNegative mp
  where
    desc = "elim (MkNonNegative x) === x"

showSpecs :: TestTree
showSpecs = testCase "Shows NonNegative" $ do
  "UnsafeNonNegative 2" @=? show (NonNeg.unsafeNonNegative @Int 2)

displaySpecs :: TestTree
displaySpecs = testCase "Displays NonNegative" $ do
  "2" @=? D.display (NonNeg.unsafeNonNegative @Int 2)
