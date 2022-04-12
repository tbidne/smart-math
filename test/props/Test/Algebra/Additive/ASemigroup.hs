module Test.Algebra.Additive.ASemigroup (props) where

import Equality (Equality (..))
import Gens qualified
import Hedgehog (Gen, PropertyName)
import Numeric.Algebra.Additive.ASemigroup (ASemigroup (..))
import Test.Tasty (TestName, TestTree)
import Test.Tasty qualified as T
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Additive Semigroup"
    [ addNumProps,
      assocProps
    ]

addNumProps :: TestTree
addNumProps =
  T.testGroup
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
  T.testGroup
    "Associativity: (x .+. y) .+. z === x .+. (y .+. z)"
    [ fractionAssoc,
      modNAssoc,
      modPAssoc,
      nonNegativeAssoc,
      positiveAssoc
    ]

fractionAssoc :: TestTree
fractionAssoc = asemigroupAssoc Gens.fraction "Fraction" "fractionAssoc"

modNAssoc :: TestTree
modNAssoc = asemigroupAssoc Gens.modN "ModN" "modNAssoc"

modPAssoc :: TestTree
modPAssoc = asemigroupAssoc Gens.modP "ModP" "modPAssoc"

nonNegativeAssoc :: TestTree
nonNegativeAssoc = asemigroupAssoc Gens.nonNegative "NonNegative" "nonNegativeAssoc"

positiveAssoc :: TestTree
positiveAssoc = asemigroupAssoc Gens.positive "Positive" "positiveAssoc"

asemigroupAssoc ::
  ( ASemigroup a,
    Show a
  ) =>
  Gen a ->
  TestName ->
  PropertyName ->
  TestTree
asemigroupAssoc = Utils.associativity (.+.)
