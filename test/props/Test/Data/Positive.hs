module Test.Data.Positive (props) where

import Hedgehog (MonadGen, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import Numeric.Algebra.Additive.ASemigroup (ASemigroup (..))
import Numeric.Algebra.Multiplicative.MGroup (MGroup (..))
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup (..))
import Numeric.Data.Positive (Positive (..))
import Numeric.Data.Positive qualified as Pos
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.TestBounds (TestBounds (..))
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Numeric.Data.Positive"
    [ mkPositiveSucceeds,
      mkPositiveFails,
      addTotal,
      multTotal,
      divTotal
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
      let py' = Pos.positiveToNonZero py
          MkPositive pz = px .%. py'
      x `div` y === pz

pos :: MonadGen m => m Int
pos = HG.integral $ HR.exponentialFrom 1 1 maxVal

nonpos :: MonadGen m => m Int
nonpos = HG.integral $ HR.exponentialFrom minVal 0 0

positive :: MonadGen m => m (Positive Int)
positive = Pos.unsafePositive <$> pos
