module Test.Data.Positive (props) where

import Data.Text.Display qualified as D
import Hedgehog (MonadGen, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import Numeric.Algebra.Additive.ASemigroup (ASemigroup ((.+.)))
import Numeric.Algebra.Multiplicative.MGroup (MGroup ((.%.)))
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup ((.*.)))
import Numeric.Data.Positive qualified as Pos
import Numeric.Data.Positive.Internal (Positive (MkPositive, UnsafePositive))
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.HUnit ((@=?))
import Test.Tasty.HUnit qualified as HUnit
import Test.TestBounds (TestBounds (maxVal, minVal))
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Numeric.Data.Positive"
    [ mkPositiveSucceeds,
      mkPositiveFails,
      testUnsafe,
      addTotal,
      multTotal,
      divTotal,
      showSpecs,
      displaySpecs
    ]

mkPositiveSucceeds :: TestTree
mkPositiveSucceeds =
  Utils.testPropertyCompat "x > 0 succeeds" "mkPositiveSucceeds" $
    H.property $ do
      x <- H.forAll pos
      Just (Pos.reallyUnsafePositive x) === Pos.mkPositive x

mkPositiveFails :: TestTree
mkPositiveFails =
  Utils.testPropertyCompat "x < 1 fails" "mkPositiveFails" $
    H.property $ do
      x <- H.forAll nonpos
      Nothing === Pos.mkPositive x

testUnsafe :: TestTree
testUnsafe = HUnit.testCase "Test unsafePositive" $ do
  UnsafePositive 5 @=? Pos.unsafePositive @Integer 5

  Utils.assertPureErrorCall expectedEx (Pos.unsafePositive @Integer 0)
  where
    expectedEx = "Numeric.Data.Positive.unsafePositive: Received value <= zero: 0"

addTotal :: TestTree
addTotal =
  Utils.testPropertyCompat "(.+.) is total" "addTotal" $
    H.property $ do
      px@(MkPositive x) <- H.forAll positive
      py@(MkPositive y) <- H.forAll positive
      let MkPositive pz = px .+. py
      x + y === pz

multTotal :: TestTree
multTotal =
  Utils.testPropertyCompat "(.*.) is total" "multTotal" $
    H.property $ do
      px@(MkPositive x) <- H.forAll positive
      py@(MkPositive y) <- H.forAll positive
      let MkPositive pz = px .*. py
      x * y === pz

divTotal :: TestTree
divTotal =
  Utils.testPropertyCompat "(.%.) is total" "divTotal" $
    H.property $ do
      px@(MkPositive x) <- H.forAll positive
      py@(MkPositive y) <- H.forAll positive
      let MkPositive pz = px .%. py
      x `div` y === pz

pos :: (MonadGen m) => m Int
pos = HG.integral $ HR.exponentialFrom 1 1 maxVal

nonpos :: (MonadGen m) => m Int
nonpos = HG.integral $ HR.exponentialFrom minVal 0 0

positive :: (MonadGen m) => m (Positive Int)
positive = Pos.unsafePositive <$> pos

showSpecs :: TestTree
showSpecs = HUnit.testCase "Shows Positive" $ do
  "UnsafePositive 2" @=? show (Pos.unsafePositive @Int 2)

displaySpecs :: TestTree
displaySpecs = HUnit.testCase "Displays Positive" $ do
  "2" @=? D.display (Pos.unsafePositive @Int 2)
