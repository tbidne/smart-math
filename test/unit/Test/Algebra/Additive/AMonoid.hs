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
      modNId,
      modPId,
      nonNegativeId
    ]

fractionId :: TestTree
fractionId = monoidIdentity Gens.fraction MkEqExact "Fraction" "fractionId"

modNId :: TestTree
modNId = monoidIdentity Gens.modN MkEqExact "ModN" "modNId"

modPId :: TestTree
modPId = monoidIdentity Gens.modP MkEqExact "ModP" "modPId"

nonNegativeId :: TestTree
nonNegativeId = monoidIdentity Gens.nonNegative MkEqExact "NonNegative" "nonNegativeId"

monoidIdentity ::
  ( AMonoid a,
    Show a
  ) =>
  Gen a ->
  (a -> Equality eq a) ->
  TestName ->
  PropertyName ->
  TestTree
monoidIdentity = Utils.identity (.+.) zero

absProps :: TestTree
absProps =
  testGroup
    "Absolute Value"
    [ fractionAbs
    ]

fractionAbs :: TestTree
fractionAbs = monoidAbs Gens.fraction MkEqExact "Fraction" "fractionAbs"

monoidAbs ::
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
monoidAbs gen eqCons desc propName =
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
