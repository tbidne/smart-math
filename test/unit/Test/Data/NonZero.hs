module Test.Data.NonZero (props) where

import Data.Text.Display qualified as D
import Hedgehog (MonadGen)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import Numeric.Data.NonZero (NonZero (MkNonZero))
import Numeric.Data.NonZero qualified as NonZero
import Numeric.Data.NonZero.Internal (NonZero (UnsafeNonZero))
import Test.Prelude
import Test.Tasty qualified as T
import Test.Tasty.HUnit qualified as HUnit
import Test.TestBounds (TestBounds (maxVal, minVal))
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Numeric.Data.NonZero"
    [ mkNonZeroSucceeds,
      mkNonZeroFails,
      testUnsafe,
      elimProps,
      showSpecs,
      displaySpecs
    ]

mkNonZeroSucceeds :: TestTree
mkNonZeroSucceeds =
  Utils.testPropertyCompat "x /= 0 succeeds" "mkNonZeroSucceeds" $
    H.property $ do
      x <- H.forAll nonzero
      Just (NonZero.reallyUnsafeNonZero x) === NonZero.mkNonZero x

mkNonZeroFails :: TestTree
mkNonZeroFails =
  Utils.testPropertyCompat "x == 0 fails" "mkNonZeroFails" $
    H.property $ do
      x <- H.forAll zero
      Nothing === NonZero.mkNonZero x

testUnsafe :: TestTree
testUnsafe = HUnit.testCase "Test unsafeNonZero" $ do
  UnsafeNonZero 5 @=? NonZero.unsafeNonZero @Integer 5

  Utils.assertPureErrorCall expectedEx (NonZero.unsafeNonZero @Integer 0)
  where
    expectedEx = "Numeric.Data.NonZero.unsafeNonZero: Received zero"

nonzero :: (MonadGen m) => m Int
nonzero =
  HG.choice
    [ HG.integral $ HR.exponentialFrom minVal 1 1,
      HG.integral $ HR.exponentialFrom 1 1 maxVal
    ]

zero :: (MonadGen m) => m Integer
zero = pure 0

genNonZero :: Gen (NonZero Int)
genNonZero = NonZero.unsafeNonZero <$> nonzero

elimProps :: TestTree
elimProps =
  Utils.testPropertyCompat desc "elimProps" $
    H.property $ do
      nz@(MkNonZero n) <- H.forAll genNonZero

      n === NonZero.unNonZero nz
      n === nz.unNonZero
      n === view #unNonZero nz
      n === view NonZero._MkNonZero nz
  where
    desc = "elim (MkNonZero x) === x"

showSpecs :: TestTree
showSpecs = HUnit.testCase "Shows Positive" $ do
  "UnsafeNonZero 2" @=? show (NonZero.unsafeNonZero @Int 2)
  "UnsafeNonZero (-3)" @=? show (NonZero.unsafeNonZero @Int (-3))

displaySpecs :: TestTree
displaySpecs = HUnit.testCase "Displays Positive" $ do
  "2" @=? D.display (NonZero.unsafeNonZero @Int 2)
  "-3" @=? D.display (NonZero.unsafeNonZero @Int (-3))
