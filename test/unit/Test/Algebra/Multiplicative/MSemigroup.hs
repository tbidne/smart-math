module Test.Algebra.Multiplicative.MSemigroup (props) where

import Equality (Equality (MkEqExact))
import Gens qualified
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup ((.*.)))
import Test.Prelude
import Utils qualified

props :: TestTree
props =
  testGroup
    "Multiplicative Semigroup"
    [ multNumProps,
      assocProps
    ]

multNumProps :: TestTree
multNumProps =
  testGroup
    "(.*.) === (*)"
    [ afractionMultNum,
      bfractionMultNum
    ]

afractionMultNum :: TestTree
afractionMultNum = msemigroupMultNum Gens.afraction MkEqExact "Fraction.Algebra" "afractionMultNum"

bfractionMultNum :: TestTree
bfractionMultNum = msemigroupMultNum Gens.bfraction MkEqExact "Fraction.Base" "bfractionMultNum"

msemigroupMultNum ::
  ( MSemigroup a,
    Num a,
    Show a
  ) =>
  Gen a ->
  (a -> Equality eq a) ->
  TestName ->
  PropertyName ->
  TestTree
msemigroupMultNum = Utils.binaryEq (*) (.*.)

assocProps :: TestTree
assocProps =
  testGroup
    "Associativity: (x .*. y) .*. z === x .*. (y .*. z)"
    [ afractionAssoc,
      bfractionAssoc,
      amodNAssoc,
      bmodNAssoc,
      amodPAssoc,
      bmodPAssoc,
      anonNegativeAssoc,
      bnonNegativeAssoc,
      apositiveAssoc,
      bpositiveAssoc
    ]

afractionAssoc :: TestTree
afractionAssoc = msemigroupAssoc Gens.afraction "Fraction.Algebra" "afractionAssoc"

bfractionAssoc :: TestTree
bfractionAssoc = msemigroupAssoc Gens.bfraction "Fraction.Base" "bfractionAssoc"

amodNAssoc :: TestTree
amodNAssoc = msemigroupAssoc Gens.amodN "ModN.Algebra" "amodNAssoc"

bmodNAssoc :: TestTree
bmodNAssoc = msemigroupAssoc Gens.bmodN "ModN.Base" "bmodNAssoc"

amodPAssoc :: TestTree
amodPAssoc = msemigroupAssoc Gens.amodP "ModP.Algebra" "amodPAssoc"

bmodPAssoc :: TestTree
bmodPAssoc = msemigroupAssoc Gens.bmodP "ModP.Base" "bmodPAssoc"

anonNegativeAssoc :: TestTree
anonNegativeAssoc = msemigroupAssoc Gens.anonNegative "NonNegative.Algebra" "anonNegativeAssoc"

bnonNegativeAssoc :: TestTree
bnonNegativeAssoc = msemigroupAssoc Gens.bnonNegative "NonNegative.Base" "bnonNegativeAssoc"

apositiveAssoc :: TestTree
apositiveAssoc = msemigroupAssoc Gens.apositive "Positive.Algebra" "apositiveAssoc"

bpositiveAssoc :: TestTree
bpositiveAssoc = msemigroupAssoc Gens.bpositive "Positive.Base" "bpositiveAssoc"

msemigroupAssoc ::
  ( Eq a,
    MSemigroup a,
    Show a
  ) =>
  Gen a ->
  TestName ->
  PropertyName ->
  TestTree
msemigroupAssoc = Utils.associativity (.*.)
