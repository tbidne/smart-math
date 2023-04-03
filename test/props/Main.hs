-- | Entrypoint for property tests.
--
-- @since 0.1
module Main (main) where

import Test.Algebra.Additive.AGroup qualified
import Test.Algebra.Additive.AMonoid qualified
import Test.Algebra.Additive.ASemigroup qualified
import Test.Algebra.Multiplicative.MGroup qualified
import Test.Algebra.Multiplicative.MMonoid qualified
import Test.Algebra.Multiplicative.MSemigroup qualified
import Test.Data.Fraction qualified
import Test.Data.Interval qualified
import Test.Data.ModN qualified
import Test.Data.ModP qualified
import Test.Data.ModP.Internal qualified
import Test.Data.NonNegative qualified
import Test.Data.NonZero qualified
import Test.Data.Positive qualified
import Test.Tasty qualified as Tasty

-- | Runs property tests. The environment variable @MAX_RUNS@ controls
-- how many test runs we do (default 100).
--
-- @since 0.1
main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.testGroup
      "Property tests"
      [ Test.Algebra.Additive.ASemigroup.props,
        Test.Algebra.Additive.AMonoid.props,
        Test.Algebra.Additive.AGroup.props,
        Test.Algebra.Multiplicative.MSemigroup.props,
        Test.Algebra.Multiplicative.MMonoid.props,
        Test.Algebra.Multiplicative.MGroup.props,
        Test.Data.Fraction.props,
        Test.Data.Interval.props,
        Test.Data.ModN.props,
        Test.Data.ModP.props,
        Test.Data.ModP.Internal.props,
        Test.Data.NonNegative.props,
        Test.Data.NonZero.props,
        Test.Data.Positive.props
      ]
