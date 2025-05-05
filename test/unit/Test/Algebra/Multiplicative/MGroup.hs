module Test.Algebra.Multiplicative.MGroup (props) where

import Equality (Equality (MkEqExact))
import Gens qualified
import Numeric.Algebra.Multiplicative.MGroup (MGroup ((.%.)))
import Numeric.Algebra.Multiplicative.MMonoid (MMonoid (one))
import Test.Prelude

props :: TestTree
props =
  testGroup
    "Multiplicative Group"
    [ divProps,
      divIdentProps
    ]

divProps :: TestTree
divProps =
  testGroup
    "(.%.) === div / (/)"
    [ fractionDiv
    ]

fractionDiv :: TestTree
fractionDiv = groupDivEq (/) Gens.fraction Gens.fractionNonZero MkEqExact "Fraction" "fractionDiv"

divIdentProps :: TestTree
divIdentProps =
  testGroup
    "Division is the inverse: one == x .%. x"
    [ fractionDivIdent,
      modPDivIdent,
      nonNegativeDivIdent,
      nonZeroDivIdent,
      positiveDivIdent
    ]

fractionDivIdent :: TestTree
fractionDivIdent = groupDivIdent Gens.fractionNonZero MkEqExact "Fraction" "fractionDivIdent"

modPDivIdent :: TestTree
modPDivIdent =
  testPropertyCompat "ModP" "modPDivIdent" $
    property $ do
      x <- forAll Gens.modPNonZero
      one === x .%. x

nonNegativeDivIdent :: TestTree
nonNegativeDivIdent = groupDivIdent Gens.nonNegativeNonZero MkEqExact "NonNegative" "nonNegativeDivIdent"

nonZeroDivIdent :: TestTree
nonZeroDivIdent = groupDivIdent Gens.nonZero MkEqExact "NonZero" "nonZeroDivIdent"

positiveDivIdent :: TestTree
positiveDivIdent =
  groupDivIdent
    Gens.positiveNonZero
    MkEqExact
    "Positive"
    "positiveDivIdent"

groupDivEq ::
  (MGroup a, Show a) =>
  (a -> a -> a) ->
  Gen a ->
  Gen a ->
  (a -> Equality eq a) ->
  TestName ->
  PropertyName ->
  TestTree
groupDivEq expectedFn gen genNZ eqCons desc propName =
  testPropertyCompat desc propName $
    property $ do
      x <- forAll gen
      d <- forAll genNZ
      let actual = x .%. d
          expected = expectedFn x d
      eqCons expected === eqCons actual

groupDivIdent ::
  (MGroup a, Show a) =>
  Gen a ->
  (a -> Equality eq a) ->
  TestName ->
  PropertyName ->
  TestTree
groupDivIdent gen eqCons desc propName =
  testPropertyCompat desc propName $
    property $ do
      x <- forAll gen
      eqCons one === eqCons (x .%. x)
