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
    [ afractionAddNum,
      bfractionAddNum
    ]

afractionAddNum :: TestTree
afractionAddNum = asemigroupAddNum Gens.afraction MkEqExact "Fraction.Algebra" "afractionAddNum"

bfractionAddNum :: TestTree
bfractionAddNum = asemigroupAddNum Gens.bfraction MkEqExact "Fraction.Base" "bfractionAddNum"

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
afractionAssoc = asemigroupAssoc Gens.afraction "Fraction.Algebra" "afractionAssoc"

bfractionAssoc :: TestTree
bfractionAssoc = asemigroupAssoc Gens.bfraction "Fraction.Base" "bfractionAssoc"

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
