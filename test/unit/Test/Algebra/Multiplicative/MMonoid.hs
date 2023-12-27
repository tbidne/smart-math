module Test.Algebra.Multiplicative.MMonoid (props) where

import Equality (Equality (..))
import Gens qualified
import Hedgehog (Gen, PropertyName)
import Numeric.Algebra.Multiplicative.MMonoid (MMonoid (..))
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup (..))
import Test.Tasty (TestName, TestTree)
import Test.Tasty qualified as T
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Multiplicative Monoid"
    [ identityProps
    ]

identityProps :: TestTree
identityProps =
  T.testGroup
    "Identity: one .*. x == x == x .*. one"
    [ fractionId,
      modNId,
      modPId,
      nonNegativeId,
      positiveId
    ]

fractionId :: TestTree
fractionId = mmonoidIdentity Gens.fraction MkEqExact "Fraction" "fractionId"

modNId :: TestTree
modNId = mmonoidIdentity Gens.modN MkEqExact "ModN" "modNId"

modPId :: TestTree
modPId = mmonoidIdentity Gens.modP MkEqExact "ModP" "modPId"

nonNegativeId :: TestTree
nonNegativeId = mmonoidIdentity Gens.nonNegative MkEqExact "NonNegative" "nonNegativeId"

positiveId :: TestTree
positiveId = mmonoidIdentity Gens.positive MkEqExact "Positive" "positiveId"

mmonoidIdentity ::
  ( MMonoid a,
    Show a
  ) =>
  Gen a ->
  (a -> Equality eq a) ->
  TestName ->
  PropertyName ->
  TestTree
mmonoidIdentity = Utils.identity (.*.) one
