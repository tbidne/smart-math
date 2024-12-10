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
      amodNAssoc,
      bmodNAssoc,
      amodPAssoc,
      bmodPAssoc,
      anonNegativeAssoc,
      bnonNegativeAssoc,
      apositiveAssoc,
      bpositiveAssoc
    ]

fractionAssoc :: TestTree
fractionAssoc = asemigroupAssoc Gens.fraction "Fraction" "fractionAssoc"

amodNAssoc :: TestTree
amodNAssoc = asemigroupAssoc Gens.amodN "ModN.Algebra" "amodNAssoc"

bmodNAssoc :: TestTree
bmodNAssoc = asemigroupAssoc Gens.amodN "ModN.Base" "bmodNAssoc"

amodPAssoc :: TestTree
amodPAssoc = asemigroupAssoc Gens.amodP "ModP.Algebra" "amodPAssoc"

bmodPAssoc :: TestTree
bmodPAssoc = asemigroupAssoc Gens.bmodP "ModP.Base" "bmodPAssoc"

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
