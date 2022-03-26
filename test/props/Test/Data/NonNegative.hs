module Test.Data.NonNegative (props) where

import Hedgehog (MonadGen, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import MaxRuns (MaxRuns (..))
import Numeric.Algebra.Additive.ASemigroup (ASemigroup (..))
import Numeric.Algebra.Multiplicative.MGroup (MGroup (..), unsafeAMonoidNonZero)
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup (..))
import Numeric.Data.NonNegative (NonNegative (..))
import Numeric.Data.NonNegative qualified as NonNeg
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.TestBounds (TestBounds (..))
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Numeric.Data.NonNegative"
    [ mkNonNegativeSucceeds,
      mkNonNegativeFails,
      addTotal,
      multTotal,
      divTotal
    ]

mkNonNegativeSucceeds :: TestTree
mkNonNegativeSucceeds = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "x >= 0 succeeds" "mkNonNegativeSucceeds" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll nonneg
        Just (NonNeg.reallyUnsafeNonNegative x) === NonNeg.mkNonNegative x

mkNonNegativeFails :: TestTree
mkNonNegativeFails = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "x < 0 fails" "mkNonNegativeFails" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll neg
        Nothing === NonNeg.mkNonNegative x

addTotal :: TestTree
addTotal = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "(.+.) is total" "addTotal" $
    H.withTests limit $
      H.property $ do
        px@(MkNonNegative x) <- H.forAll nonnegative
        py@(MkNonNegative y) <- H.forAll nonnegative
        let MkNonNegative pz = px .+. py
        x + y === pz

multTotal :: TestTree
multTotal = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "(.*.) is total" "multTotal" $
    H.withTests limit $
      H.property $ do
        px@(MkNonNegative x) <- H.forAll nonnegative
        py@(MkNonNegative y) <- H.forAll nonnegative
        let MkNonNegative pz = px .*. py
        x * y === pz

divTotal :: TestTree
divTotal = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "(.%.) is total" "divTotal" $
    H.withTests limit $
      H.property $ do
        px@(MkNonNegative x) <- H.forAll nonnegative
        py@(MkNonNegative y) <- H.forAll nonnegativeNZ
        let MkNonNegative pz = px .%. unsafeAMonoidNonZero py
        x `div` y === pz

nonneg :: MonadGen m => m Int
nonneg = HG.integral $ HR.exponentialFrom 0 0 maxVal

neg :: MonadGen m => m Int
neg = HG.integral $ HR.exponentialFrom minVal -1 -1

nonnegative :: MonadGen m => m (NonNegative Int)
nonnegative = NonNeg.unsafeNonNegative <$> nonneg

nonnegativeNZ :: MonadGen m => m (NonNegative Int)
nonnegativeNZ = NonNeg.unsafeNonNegative <$> HG.integral (HR.exponentialFrom 1 1 maxVal)
