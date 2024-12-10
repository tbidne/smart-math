module Test.Algebra.Additive.AMonoid (props) where

import Equality (Equality (MkEqExact))
import Gens qualified
import Numeric.Algebra.Additive.AMonoid (AMonoid (zero))
import Numeric.Algebra.Additive.ASemigroup (ASemigroup ((.+.)))
import Numeric.Algebra.Normed (Normed (norm))
import Test.Prelude
import Utils ((<=>))
import Utils qualified

props :: TestTree
props =
  testGroup
    "Additive Monoid"
    [ identityProps,
      absProps
    ]

identityProps :: TestTree
identityProps =
  testGroup
    "Identity: zero .+. x == x == x .+. zero"
    [ fractionId,
      amodNId,
      bmodNId,
      amodPId,
      bmodPId,
      anonNegativeId,
      bnonNegativeId
    ]

fractionId :: TestTree
fractionId = amonoidIdentity Gens.fraction MkEqExact "Fraction" "fractionId"

amodNId :: TestTree
amodNId = amonoidIdentity Gens.amodN MkEqExact "ModN.Algebra" "amodNId"

bmodNId :: TestTree
bmodNId = amonoidIdentity Gens.bmodN MkEqExact "ModN.Base" "bmodNId"

amodPId :: TestTree
amodPId = amonoidIdentity Gens.amodP MkEqExact "ModP.Algebra" "amodPId"

bmodPId :: TestTree
bmodPId = amonoidIdentity Gens.bmodP MkEqExact "ModP.Base" "bmodPId"

anonNegativeId :: TestTree
anonNegativeId = amonoidIdentity Gens.anonNegative MkEqExact "NonNegative.Algebra" "anonNegativeId"

bnonNegativeId :: TestTree
bnonNegativeId = amonoidIdentity Gens.bnonNegative MkEqExact "NonNegative.Base" "bnonNegativeId"

amonoidIdentity ::
  ( AMonoid a,
    Show a
  ) =>
  Gen a ->
  (a -> Equality eq a) ->
  TestName ->
  PropertyName ->
  TestTree
amonoidIdentity = Utils.identity (.+.) zero

absProps :: TestTree
absProps =
  testGroup
    "Absolute Value"
    [ fractionAbs
    ]

fractionAbs :: TestTree
fractionAbs = amonoidAbs Gens.fraction MkEqExact "Fraction" "fractionAbs"

amonoidAbs ::
  ( AMonoid a,
    Normed a,
    Ord a,
    Show a
  ) =>
  Gen a ->
  (a -> Equality eq a) ->
  TestName ->
  PropertyName ->
  TestTree
amonoidAbs gen eqCons desc propName =
  testPropertyCompat desc propName $
    property $ do
      x <- forAll gen
      y <- forAll gen

      -- idempotence: |x| = ||x||
      let eqX = eqCons x
          eqAbs = eqCons (norm x)
      eqAbs === eqCons (norm (norm x))

      -- non-negative: |x| >= 0
      let eqZero = eqCons zero
      diff eqAbs (>=) eqZero

      -- positive-definite: |x| == 0 <=> x == 0
      diff (eqAbs == eqZero) (<=>) (eqX == eqZero)

      -- triangle equality: |x + y| <= |x| + |y|
      let sumAbs = eqCons $ norm x .+. norm y
          absSum = eqCons $ norm (x .+. y)
      diff absSum (<=) sumAbs
