module Test.Algebra.Additive.ASemigroup (props) where

import Equality (Equality (MkEqExact))
import Gens qualified
import Numeric.Algebra.Additive.ASemigroup (ASemigroup ((.+.)))
import Test.Prelude
import Utils qualified

props :: TestTree
props =
  testGroup
    "Additive Semigroup"
    [ addNumProps,
      assocProps
    ]

addNumProps :: TestTree
addNumProps =
  testGroup
    "(.+.) === (+)"
    [ fractionAddNum
    ]

fractionAddNum :: TestTree
fractionAddNum = asemigroupAddNum Gens.fraction MkEqExact "Fraction" "fractionAddNum"

asemigroupAddNum ::
  ( ASemigroup a,
    Num a,
    Show a
  ) =>
  Gen a ->
  (a -> Equality eq a) ->
  TestName ->
  PropertyName ->
  TestTree
asemigroupAddNum = Utils.binaryEq (+) (.+.)

assocProps :: TestTree
assocProps =
  testGroup
    "Associativity: (x .+. y) .+. z === x .+. (y .+. z)"
    [ fractionAssoc,
      modNAssoc,
      modPAssoc,
      anonNegativeAssoc,
      bnonNegativeAssoc,
      apositiveAssoc,
      bpositiveAssoc
    ]

fractionAssoc :: TestTree
fractionAssoc = asemigroupAssoc Gens.fraction "Fraction" "fractionAssoc"

modNAssoc :: TestTree
modNAssoc = asemigroupAssoc Gens.modN "ModN" "modNAssoc"

modPAssoc :: TestTree
modPAssoc = asemigroupAssoc Gens.modP "ModP" "modPAssoc"

anonNegativeAssoc :: TestTree
anonNegativeAssoc = asemigroupAssoc Gens.anonNegative "NonNegative.Algebra" "anonNegativeAssoc"

bnonNegativeAssoc :: TestTree
bnonNegativeAssoc = asemigroupAssoc Gens.bnonNegative "NonNegative.Base" "bnonNegativeAssoc"

apositiveAssoc :: TestTree
apositiveAssoc = asemigroupAssoc Gens.apositive "Positive.Algebra" "apositiveAssoc"

bpositiveAssoc :: TestTree
bpositiveAssoc = asemigroupAssoc Gens.bpositive "Positive.Base" "bpositiveAssoc"

asemigroupAssoc ::
  ( ASemigroup a,
    Eq a,
    Show a
  ) =>
  Gen a ->
  TestName ->
  PropertyName ->
  TestTree
asemigroupAssoc = Utils.associativity (.+.)
