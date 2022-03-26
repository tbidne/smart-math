module Test.Algebra.Additive.AGroup (props) where

import Equality (Equality (..))
import Gens qualified
import Hedgehog (Gen, PropertyName, (===))
import Hedgehog qualified as H
import MaxRuns (MaxRuns (..))
import Numeric.Algebra.Additive.AGroup (AGroup (..), aabs)
import Numeric.Algebra.Additive.AMonoid (AMonoid (..))
import Numeric.Algebra.Additive.ASemigroup (ASemigroup (..))
import Test.Tasty (TestName, TestTree)
import Test.Tasty qualified as T
import Utils ((<=>))
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Additive Group"
    [ subProps,
      subIdentProps,
      absProps
    ]

subProps :: TestTree
subProps =
  T.testGroup
    "(.-.) === (-)"
    [ fractionSub
    ]

fractionSub :: TestTree
fractionSub = agroupSubEq Gens.fraction MkEqExact "Fraction" "fractionSub"

subIdentProps :: TestTree
subIdentProps =
  T.testGroup
    "Subtraction is the inverse: zero == x .-. x"
    [ fractionSubIdent,
      modNSubIdent,
      modPSubIdent
    ]

fractionSubIdent :: TestTree
fractionSubIdent = agroupSubIdent Gens.fraction "Fraction" "fractionSubIdent"

modNSubIdent :: TestTree
modNSubIdent = agroupSubIdent Gens.modN "ModN" "modNSubIdent"

modPSubIdent :: TestTree
modPSubIdent = agroupSubIdent Gens.modP "ModP" "modPSubIdent"

absProps :: TestTree
absProps =
  T.testGroup
    "Absolute Value"
    [ fractionAbs,
      modNAbs,
      modPAbs
    ]

fractionAbs :: TestTree
fractionAbs = agroupAbs Gens.fraction MkEqExact "Fraction" "fractionAbs"

modNAbs :: TestTree
modNAbs = agroupAbs Gens.modN MkEqExact "ModN" "modNAbs"

modPAbs :: TestTree
modPAbs = agroupAbs Gens.modP MkEqExact "ModP" "modPAbs"

agroupSubEq ::
  ( SubtractConstraint a ~ a,
    AGroup a,
    Num a,
    Show a
  ) =>
  Gen a ->
  (a -> Equality eq a) ->
  TestName ->
  PropertyName ->
  TestTree
agroupSubEq = Utils.binaryEq (-) (.-.)

agroupAbs ::
  ( AddConstraint a ~ a,
    AGroup a,
    Ord a,
    Show a
  ) =>
  Gen a ->
  (a -> Equality eq a) ->
  TestName ->
  PropertyName ->
  TestTree
agroupAbs gen eqCons desc propName = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat desc propName $
    H.withTests limit $
      H.property $ do
        x <- H.forAll gen
        y <- H.forAll gen

        -- idempotence: |x| = ||x||
        let eqX = eqCons x
            eqAbs = eqCons (aabs x)
        eqAbs === eqCons (aabs (aabs x))

        -- non-negative: |x| >= 0
        let eqZero = eqCons zero
        H.diff eqAbs (>=) eqZero

        -- positive-definite: |x| == 0 <=> x == 0
        H.diff (eqAbs == eqZero) (<=>) (eqX == eqZero)

        -- triangle equality: |x + y| <= |x| + |y|
        let sumAbs = eqCons $ aabs x .+. aabs y
            absSum = eqCons $ aabs (x .+. y)
        H.diff absSum (<=) sumAbs

agroupSubIdent ::
  ( SubtractConstraint a ~ a,
    AGroup a,
    Show a
  ) =>
  Gen a ->
  TestName ->
  PropertyName ->
  TestTree
agroupSubIdent gen desc propName = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat desc propName $
    H.withTests limit $
      H.property $ do
        x <- H.forAll gen
        zero === x .-. x
