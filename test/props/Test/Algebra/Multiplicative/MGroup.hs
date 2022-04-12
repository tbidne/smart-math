module Test.Algebra.Multiplicative.MGroup (props) where

import Equality (Equality (..))
import Gens qualified
import Hedgehog (Gen, PropertyName, (===))
import Hedgehog qualified as H
import MaxRuns (MaxRuns (..))
import Numeric.Algebra.Multiplicative.MGroup (MGroup (..), NonZero (..))
import Numeric.Algebra.Multiplicative.MMonoid (MMonoid (..))
import Test.Tasty (TestName, TestTree)
import Test.Tasty qualified as T
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Multiplicative Group"
    [ divProps,
      divIdentProps
    ]

divProps :: TestTree
divProps =
  T.testGroup
    "(.%.) === div / (/)"
    [ fractionDiv
    ]

fractionDiv :: TestTree
fractionDiv = mgroupDivEq (/) Gens.fraction Gens.fractionNonZero MkEqExact "Fraction" "fractionDiv"

divIdentProps :: TestTree
divIdentProps =
  T.testGroup
    "Division is the inverse: one == x .%. x"
    [ fractionDivIdent,
      modPDivIdent,
      nonNegativeDivIdent,
      nonZeroDivIdent,
      positiveDivIdent
    ]

fractionDivIdent :: TestTree
fractionDivIdent = agroupDivIdent Gens.fractionNonZero MkEqExact "Fraction" "fractionDivIdent"

modPDivIdent :: TestTree
modPDivIdent = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "ModP" "modPDivIdent" $
    H.withTests limit $
      H.property $ do
        nz@(MkNonZero x) <- H.forAll Gens.modPNonZero
        one === x .%. nz

nonNegativeDivIdent :: TestTree
nonNegativeDivIdent = agroupDivIdent Gens.nonNegativeNonZero MkEqExact "NonNegative" "nonNegativeDivIdent"

nonZeroDivIdent :: TestTree
nonZeroDivIdent = agroupDivIdent Gens.nonZero MkEqExact "NonZero" "nonZeroDivIdent"

positiveDivIdent :: TestTree
positiveDivIdent = agroupDivIdent Gens.positiveNonZero MkEqExact "Positive" "positiveDivIdent"

mgroupDivEq ::
  (MGroup a, Show a) =>
  (a -> a -> a) ->
  Gen a ->
  Gen (NonZero a) ->
  (a -> Equality eq a) ->
  TestName ->
  PropertyName ->
  TestTree
mgroupDivEq expectedFn gen genNZ eqCons desc propName = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat desc propName $
    H.withTests limit $
      H.property $ do
        x <- H.forAll gen
        nz@(MkNonZero d) <- H.forAll genNZ
        let actual = x .%. nz
            expected = expectedFn x d
        eqCons expected === eqCons actual

agroupDivIdent ::
  (MGroup a, Show a) =>
  Gen (NonZero a) ->
  (a -> Equality eq a) ->
  TestName ->
  PropertyName ->
  TestTree
agroupDivIdent gen eqCons desc propName = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat desc propName $
    H.withTests limit $
      H.property $ do
        nz@(MkNonZero x) <- H.forAll gen
        eqCons one === eqCons (x .%. nz)
