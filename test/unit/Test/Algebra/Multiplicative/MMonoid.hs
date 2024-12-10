module Test.Algebra.Multiplicative.MMonoid (props) where

import Equality (Equality (MkEqExact))
import Gens qualified
import Numeric.Algebra.Multiplicative.MMonoid (MMonoid (one))
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup ((.*.)))
import Test.Prelude
import Utils qualified

props :: TestTree
props =
  testGroup
    "Multiplicative Monoid"
    [ identityProps
    ]

identityProps :: TestTree
identityProps =
  testGroup
    "Identity: one .*. x == x == x .*. one"
    [ fractionId,
      modNId,
      modPId,
      anonNegativeId,
      bnonNegativeId,
      apositiveId,
      bpositiveId
    ]

fractionId :: TestTree
fractionId = mmonoidIdentity Gens.fraction MkEqExact "Fraction" "fractionId"

modNId :: TestTree
modNId = mmonoidIdentity Gens.modN MkEqExact "ModN" "modNId"

modPId :: TestTree
modPId = mmonoidIdentity Gens.modP MkEqExact "ModP" "modPId"

anonNegativeId :: TestTree
anonNegativeId = mmonoidIdentity Gens.anonNegative MkEqExact "NonNegative.Algebra" "anonNegativeId"

bnonNegativeId :: TestTree
bnonNegativeId = mmonoidIdentity Gens.bnonNegative MkEqExact "NonNegative.Base" "bnonNegativeId"

apositiveId :: TestTree
apositiveId = mmonoidIdentity Gens.apositive MkEqExact "Positive.Algebra" "apositiveId"

bpositiveId :: TestTree
bpositiveId = mmonoidIdentity Gens.bpositive MkEqExact "Positive.Base" "bpositiveId"

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
