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
    [ fractionMultNum
    ]

fractionMultNum :: TestTree
fractionMultNum = msemigroupMultNum Gens.fraction MkEqExact "Fraction" "fractionMultNum"

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
    [ fractionAssoc,
      modNAssoc,
      modPAssoc,
      anonNegativeAssoc,
      bnonNegativeAssoc,
      apositiveAssoc,
      bpositiveAssoc
    ]

fractionAssoc :: TestTree
fractionAssoc = msemigroupAssoc Gens.fraction "Fraction" "fractionAssoc"

modNAssoc :: TestTree
modNAssoc = msemigroupAssoc Gens.modN "ModN" "modNAssoc"

modPAssoc :: TestTree
modPAssoc = msemigroupAssoc Gens.modN "ModP" "modPAssoc"

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
