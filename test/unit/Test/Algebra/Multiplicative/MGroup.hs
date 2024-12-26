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
    [ afractionDiv,
      bfractionDiv
    ]

afractionDiv :: TestTree
afractionDiv = mgroupDivEq (/) Gens.afraction Gens.afractionNonZero MkEqExact "Fraction.Algebra" "afractionDiv"

bfractionDiv :: TestTree
bfractionDiv = mgroupDivEq (/) Gens.bfraction Gens.bfractionNonZero MkEqExact "Fraction.Base" "bfractionDiv"

divIdentProps :: TestTree
divIdentProps =
  testGroup
    "Division is the inverse: one == x .%. x"
    [ afractionDivIdent,
      bfractionDivIdent,
      amodPDivIdent,
      bmodPDivIdent,
      anonNegativeDivIdent,
      bnonNegativeDivIdent,
      anonZeroDivIdent,
      bnonZeroDivIdent,
      apositiveDivIdent,
      bpositiveDivIdent
    ]

afractionDivIdent :: TestTree
afractionDivIdent = agroupDivIdent Gens.afractionNonZero MkEqExact "Fraction.Algebra" "afractionDivIdent"

bfractionDivIdent :: TestTree
bfractionDivIdent = agroupDivIdent Gens.bfractionNonZero MkEqExact "Fraction.Base" "bfractionDivIdent"

amodPDivIdent :: TestTree
amodPDivIdent =
  testPropertyCompat "ModP.Algebra" "amodPDivIdent" $
    property $ do
      x <- forAll Gens.amodPNonZero
      one === x .%. x

bmodPDivIdent :: TestTree
bmodPDivIdent =
  testPropertyCompat "ModP.Base" "bmodPDivIdent" $
    property $ do
      x <- forAll Gens.bmodPNonZero
      one === x .%. x

anonNegativeDivIdent :: TestTree
anonNegativeDivIdent = agroupDivIdent Gens.anonNegativeNonZero MkEqExact "NonNegative.Algebra" "anonNegativeDivIdent"

bnonNegativeDivIdent :: TestTree
bnonNegativeDivIdent = agroupDivIdent Gens.bnonNegativeNonZero MkEqExact "NonNegative.Base" "bnonNegativeDivIdent"

anonZeroDivIdent :: TestTree
anonZeroDivIdent = agroupDivIdent Gens.anonZero MkEqExact "NonZero.Algebra" "anonZeroDivIdent"

bnonZeroDivIdent :: TestTree
bnonZeroDivIdent = agroupDivIdent Gens.bnonZero MkEqExact "NonZero.Base" "bnonZeroDivIdent"

apositiveDivIdent :: TestTree
apositiveDivIdent =
  agroupDivIdent
    Gens.apositiveNonZero
    MkEqExact
    "Positive.Algebra"
    "apositiveDivIdent"

bpositiveDivIdent :: TestTree
bpositiveDivIdent =
  agroupDivIdent
    Gens.bpositiveNonZero
    MkEqExact
    "Positive.Base"
    "bpositiveDivIdent"

mgroupDivEq ::
  (MGroup a, Show a) =>
  (a -> a -> a) ->
  Gen a ->
  Gen a ->
  (a -> Equality eq a) ->
  TestName ->
  PropertyName ->
  TestTree
mgroupDivEq expectedFn gen genNZ eqCons desc propName =
  testPropertyCompat desc propName $
    property $ do
      x <- forAll gen
      d <- forAll genNZ
      let actual = x .%. d
          expected = expectedFn x d
      eqCons expected === eqCons actual

agroupDivIdent ::
  (MGroup a, Show a) =>
  Gen a ->
  (a -> Equality eq a) ->
  TestName ->
  PropertyName ->
  TestTree
agroupDivIdent gen eqCons desc propName =
  testPropertyCompat desc propName $
    property $ do
      x <- forAll gen
      eqCons one === eqCons (x .%. x)
