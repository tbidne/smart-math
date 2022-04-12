module Test.Algebra.Multiplicative.MSemigroup (props) where

import Equality (Equality (..))
import Gens qualified
import Hedgehog (Gen, PropertyName)
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup (..))
import Test.Tasty (TestName, TestTree)
import Test.Tasty qualified as T
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Multiplicative Semigroup"
    [ multNumProps,
      assocProps
    ]

multNumProps :: TestTree
multNumProps =
  T.testGroup
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
  T.testGroup
    "Associativity: (x .*. y) .*. z === x .*. (y .*. z)"
    [ fractionAssoc,
      modNAssoc,
      modPAssoc,
      nonNegativeAssoc,
      positiveAssoc
    ]

fractionAssoc :: TestTree
fractionAssoc = msemigroupAssoc Gens.fraction "Fraction" "fractionAssoc"

modNAssoc :: TestTree
modNAssoc = msemigroupAssoc Gens.modN "ModN" "modNAssoc"

modPAssoc :: TestTree
modPAssoc = msemigroupAssoc Gens.modN "ModP" "modPAssoc"

nonNegativeAssoc :: TestTree
nonNegativeAssoc = msemigroupAssoc Gens.nonNegative "NonNegative" "nonNegativeAssoc"

positiveAssoc :: TestTree
positiveAssoc = msemigroupAssoc Gens.positive "Positive" "positiveAssoc"

msemigroupAssoc ::
  ( MSemigroup a,
    Show a
  ) =>
  Gen a ->
  TestName ->
  PropertyName ->
  TestTree
msemigroupAssoc = Utils.associativity (.*.)
