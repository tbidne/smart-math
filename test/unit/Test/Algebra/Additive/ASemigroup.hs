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
fractionAddNum = semigroupAddNum Gens.fraction MkEqExact "Fraction" "fractionAddNum"

semigroupAddNum ::
  ( ASemigroup a,
    Num a,
    Show a
  ) =>
  Gen a ->
  (a -> Equality eq a) ->
  TestName ->
  PropertyName ->
  TestTree
semigroupAddNum = Utils.binaryEq (+) (.+.)

assocProps :: TestTree
assocProps =
  testGroup
    "Associativity: (x .+. y) .+. z === x .+. (y .+. z)"
    [ fractionAssoc,
      modNAssoc,
      modPAssoc,
      nonNegativeAssoc,
      positiveAssoc
    ]

fractionAssoc :: TestTree
fractionAssoc = semigroupAssoc Gens.fraction "Fraction" "fractionAssoc"

modNAssoc :: TestTree
modNAssoc = semigroupAssoc Gens.modN "ModN" "modNAssoc"

modPAssoc :: TestTree
modPAssoc = semigroupAssoc Gens.modP "ModP" "modPAssoc"

nonNegativeAssoc :: TestTree
nonNegativeAssoc = semigroupAssoc Gens.nonNegative "NonNegative" "nonNegativeAssoc"

positiveAssoc :: TestTree
positiveAssoc = semigroupAssoc Gens.positive "Positive" "positiveAssoc"

semigroupAssoc ::
  ( ASemigroup a,
    Eq a,
    Show a
  ) =>
  Gen a ->
  TestName ->
  PropertyName ->
  TestTree
semigroupAssoc = Utils.associativity (.+.)
