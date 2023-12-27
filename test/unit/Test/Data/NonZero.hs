module Test.Data.NonZero (props) where

import Hedgehog (MonadGen, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import Numeric.Data.NonZero qualified as NonZero
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.TestBounds (TestBounds (..))
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Numeric.Data.NonZero"
    [ mkNonZeroSucceeds,
      mkNonZeroFails
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

nonzero :: (MonadGen m) => m Int
nonzero =
  HG.choice
    [ HG.integral $ HR.exponentialFrom minVal 1 1,
      HG.integral $ HR.exponentialFrom 1 1 maxVal
    ]

zero :: (MonadGen m) => m Integer
zero = pure 0
