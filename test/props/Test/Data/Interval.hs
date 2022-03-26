module Test.Data.Interval (props) where

import Hedgehog (MonadGen, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import MaxRuns (MaxRuns (..))
import Numeric.Data.Interval qualified as Interval
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Numeric.Data.Interval"
    [ mkLRIntervalSucceeds,
      mkLRIntervalFailsLeft,
      mkLRIntervalFailsRight,
      mkLIntervalSucceeds,
      mkLIntervalFails,
      mkRIntervalSucceeds,
      mkRIntervalFails
    ]

mkLRIntervalSucceeds :: TestTree
mkLRIntervalSucceeds = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "x in [500, 10,000] succeeds LRInterval 500 10,000" "mkLRIntervalSucceeds" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll inRange
        maybe H.failure (const (pure ())) (Interval.mkLRInterval @500 @1_000 x)

mkLRIntervalFailsLeft :: TestTree
mkLRIntervalFailsLeft = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "x < 500 fails LRInterval 500 1,000" "mkLRIntervalFailsLeft" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll lt500
        Nothing === Interval.mkLRInterval @500 @1_000 x

mkLRIntervalFailsRight :: TestTree
mkLRIntervalFailsRight = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "x > 1,000 fails LRInterval 500 1,000" "mkLRIntervalFailsRight" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll gt1000
        Nothing === Interval.mkLRInterval @500 @1_000 x

mkLIntervalSucceeds :: TestTree
mkLIntervalSucceeds = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "x > 1,000 succeeds LInterval 1,000" "mkLIntervalSucceeds" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll gt1000
        maybe H.failure (const (pure ())) (Interval.mkLInterval @1_000 x)

mkLIntervalFails :: TestTree
mkLIntervalFails = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "x < 500 fails LInterval 500" "mkLIntervalFails" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll lt500
        Nothing === Interval.mkLInterval @500 x

mkRIntervalSucceeds :: TestTree
mkRIntervalSucceeds = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "x < 500 succeeds RInterval 500" "mkRIntervalSucceeds" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll lt500
        maybe H.failure (const (pure ())) (Interval.mkRInterval @500 x)

mkRIntervalFails :: TestTree
mkRIntervalFails = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "x > 10,000 fails RInterval 1,000" "mkRIntervalFails" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll gt1000
        Nothing === Interval.mkRInterval @1_000 x

gt1000 :: MonadGen m => m Int
gt1000 = HG.integral $ HR.linearFrom 1_001 1_001 2_000

lt500 :: MonadGen m => m Int
lt500 = HG.integral $ HR.linearFrom 0 0 (500 - 1)

inRange :: MonadGen m => m Int
inRange = HG.integral $ HR.exponentialFrom 500 500 1_000
