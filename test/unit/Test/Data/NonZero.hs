module Test.Data.NonZero (props) where

import Data.Text.Display qualified as D
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import Numeric.Data.NonZero (NonZero (MkNonZero))
import Numeric.Data.NonZero qualified as NonZero
import Numeric.Data.NonZero.Internal (NonZero (UnsafeNonZero))
import Test.Prelude
import Test.TestBounds (TestBounds (maxVal, minVal))
import Utils qualified

props :: TestTree
props =
  testGroup
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
  testPropertyCompat "x /= 0 succeeds" "mkNonZeroSucceeds" $
    property $ do
      x <- forAll nonzero
      Just (NonZero.reallyUnsafeNonZero x) === NonZero.mkNonZero x

mkNonZeroFails :: TestTree
mkNonZeroFails =
  testPropertyCompat "x == 0 fails" "mkNonZeroFails" $
    property $ do
      x <- forAll zero
      Nothing === NonZero.mkNonZero x

testUnsafe :: TestTree
testUnsafe = testCase "Test unsafeNonZero" $ do
  UnsafeNonZero 5 @=? NonZero.unsafeNonZero @Integer 5

  Utils.assertPureErrorCall expectedEx (NonZero.unsafeNonZero @Integer 0)
  where
    expectedEx = "Numeric.Data.NonZero.unsafeNonZero: Received zero"

nonzero :: Gen Int
nonzero =
  HG.choice
    [ HG.integral $ HR.exponentialFrom minVal 1 1,
      HG.integral $ HR.exponentialFrom 1 1 maxVal
    ]

zero :: Gen Integer
zero = pure 0

genNonZero :: Gen (NonZero Int)
genNonZero = NonZero.unsafeNonZero <$> nonzero

elimProps :: TestTree
elimProps =
  testPropertyCompat desc "elimProps" $
    property $ do
      nz@(MkNonZero n) <- forAll genNonZero

      n === NonZero.unNonZero nz
      n === nz.unNonZero
      n === view #unNonZero nz
      n === view NonZero._MkNonZero nz
  where
    desc = "elim (MkNonZero x) === x"

showSpecs :: TestTree
showSpecs = testCase "Shows Positive" $ do
  "UnsafeNonZero 2" @=? show (NonZero.unsafeNonZero @Int 2)
  "UnsafeNonZero (-3)" @=? show (NonZero.unsafeNonZero @Int (-3))

displaySpecs :: TestTree
displaySpecs = testCase "Displays Positive" $ do
  "2" @=? D.display (NonZero.unsafeNonZero @Int 2)
  "-3" @=? D.display (NonZero.unsafeNonZero @Int (-3))
