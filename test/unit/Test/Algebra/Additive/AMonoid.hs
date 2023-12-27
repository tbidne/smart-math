module Test.Algebra.Additive.AMonoid (props) where

import Equality (Equality (MkEqExact))
import Gens qualified
import Hedgehog (Gen, PropertyName, (===))
import Hedgehog qualified as H
import Numeric.Algebra.Additive.AMonoid (AMonoid (zero))
import Numeric.Algebra.Additive.ASemigroup (ASemigroup ((.+.)))
import Numeric.Algebra.Normed (Normed (norm))
import Test.Tasty (TestName, TestTree)
import Test.Tasty qualified as T
import Utils ((<=>))
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Additive Monoid"
    [ identityProps,
      absProps
    ]

identityProps :: TestTree
identityProps =
  T.testGroup
    "Identity: zero .+. x == x == x .+. zero"
    [ fractionId,
      modNId,
      modPId,
      nonNegativeId
    ]

fractionId :: TestTree
fractionId = amonoidIdentity Gens.fraction MkEqExact "Fraction" "fractionId"

modNId :: TestTree
modNId = amonoidIdentity Gens.modN MkEqExact "ModN" "modNId"

modPId :: TestTree
modPId = amonoidIdentity Gens.modN MkEqExact "ModP" "modPId"

nonNegativeId :: TestTree
nonNegativeId = amonoidIdentity Gens.nonNegative MkEqExact "NonNegative" "nonNegativeId"

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
  T.testGroup
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
  Utils.testPropertyCompat desc propName $
    H.property $ do
      x <- H.forAll gen
      y <- H.forAll gen

      -- idempotence: |x| = ||x||
      let eqX = eqCons x
          eqAbs = eqCons (norm x)
      eqAbs === eqCons (norm (norm x))

      -- non-negative: |x| >= 0
      let eqZero = eqCons zero
      H.diff eqAbs (>=) eqZero

      -- positive-definite: |x| == 0 <=> x == 0
      H.diff (eqAbs == eqZero) (<=>) (eqX == eqZero)

      -- triangle equality: |x + y| <= |x| + |y|
      let sumAbs = eqCons $ norm x .+. norm y
          absSum = eqCons $ norm (x .+. y)
      H.diff absSum (<=) sumAbs
