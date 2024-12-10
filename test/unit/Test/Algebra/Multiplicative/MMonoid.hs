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
      amodNId,
      bmodNId,
      amodPId,
      bmodPId,
      anonNegativeId,
      bnonNegativeId,
      apositiveId,
      bpositiveId
    ]

fractionId :: TestTree
fractionId = mmonoidIdentity Gens.fraction MkEqExact "Fraction" "fractionId"

amodNId :: TestTree
amodNId = mmonoidIdentity Gens.amodN MkEqExact "ModN.Algebra" "amodNId"

bmodNId :: TestTree
bmodNId = mmonoidIdentity Gens.bmodN MkEqExact "ModN.Base" "bmodNId"

amodPId :: TestTree
amodPId = mmonoidIdentity Gens.amodP MkEqExact "ModP.Algebra" "amodPId"

bmodPId :: TestTree
bmodPId = mmonoidIdentity Gens.bmodP MkEqExact "ModP.Base" "bmodPId"

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
