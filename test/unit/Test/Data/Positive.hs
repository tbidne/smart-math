module Test.Data.Positive (props) where

import Data.Text.Display qualified as D
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import Numeric.Algebra.Additive.ASemigroup (ASemigroup ((.+.)))
import Numeric.Algebra.Multiplicative.MGroup (MGroup ((.%.)))
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup ((.*.)))
import Numeric.Data.Positive qualified as Pos
import Numeric.Data.Positive.Internal qualified as PosI
import Test.Prelude
import Test.TestBounds (TestBounds (maxVal, minVal))
import Utils qualified

props :: TestTree
props =
  testGroup
    "Numeric.Data.Positive"
    [ mkPositiveSucceeds,
      mkPositiveFails,
      testUnsafe,
      addTotal,
      multTotal,
      divTotal,
      elimProps,
      showSpecs,
      displaySpecs
    ]

mkPositiveSucceeds :: TestTree
mkPositiveSucceeds =
  testPropertyCompat "x > 0 succeeds" "mkPositiveSucceeds" $
    property $ do
      x <- forAll pos
      Just (Pos.reallyUnsafePositive x) === Pos.mkPositive x

mkPositiveFails :: TestTree
mkPositiveFails =
  testPropertyCompat "x < 1 fails" "mkPositiveFails" $
    property $ do
      x <- forAll nonpos
      Nothing === Pos.mkPositive x

testUnsafe :: TestTree
testUnsafe = testCase "Test unsafePositive" $ do
  PosI.UnsafePositive 5 @=? Pos.unsafePositive @Integer 5

  Utils.assertPureErrorCall expectedEx (Pos.unsafePositive @Integer 0)
  where
    expectedEx = "Numeric.Data.Positive.unsafePositive: Received value <= zero: 0"

addTotal :: TestTree
addTotal =
  testPropertyCompat "(.+.) is total" "addTotal" $
    property $ do
      px@(Pos.MkPositive x) <- forAll positive
      py@(Pos.MkPositive y) <- forAll positive
      let Pos.MkPositive pz = px .+. py
      x + y === pz

multTotal :: TestTree
multTotal =
  testPropertyCompat "(.*.) is total" "multTotal" $
    property $ do
      px@(Pos.MkPositive x) <- forAll positive
      py@(Pos.MkPositive y) <- forAll positive
      let Pos.MkPositive pz = px .*. py
      x * y === pz

divTotal :: TestTree
divTotal =
  testPropertyCompat "(.%.) is total" "divTotal" $
    property $ do
      px@(Pos.MkPositive x) <- forAll positive
      py@(Pos.MkPositive y) <- forAll positive
      let Pos.MkPositive pz = px .%. py
      x `div` y === pz

pos :: Gen Int
pos = HG.integral $ HR.exponentialFrom 1 1 maxVal

nonpos :: Gen Int
nonpos = HG.integral $ HR.exponentialFrom minVal 0 0

positive :: Gen (Pos.Positive Int)
positive = Pos.unsafePositive <$> pos

elimProps :: TestTree
elimProps =
  testPropertyCompat desc "elimProps" $
    property $ do
      nz@(Pos.MkPositive n) <- forAll positive

      n === Pos.unPositive nz
      n === nz.unPositive
      n === view #unPositive nz
      n === view Pos._MkPositive nz
  where
    desc = "elim (MkPositive x) === x"

showSpecs :: TestTree
showSpecs = testCase "Shows Positive" $ do
  "UnsafePositive 2" @=? show (Pos.unsafePositive @Int 2)

displaySpecs :: TestTree
displaySpecs = testCase "Displays Positive" $ do
  "2" @=? D.display (Pos.unsafePositive @Int 2)
