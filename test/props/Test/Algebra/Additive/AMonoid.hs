{-# LANGUAGE OverloadedStrings #-}

module Test.Algebra.Additive.AMonoid (props) where

import Equality (Equality (..))
import Gens qualified
import Hedgehog (Gen, PropertyName)
import Numeric.Algebra.Additive.AMonoid (AMonoid (..))
import Numeric.Algebra.Additive.ASemigroup (ASemigroup (..))
import Test.Tasty (TestName, TestTree)
import Test.Tasty qualified as T
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Additive Monoid"
    [ identityProps
    ]

identityProps :: TestTree
identityProps =
  T.testGroup
    "Identity: zero .+. x == x == x .+. zero"
    [ fractionId,
      modNId,
      modPId,
      nonNegativeId
    ]

fractionId :: TestTree
fractionId = amonoidIdentity Gens.fraction MkEqExact "Fraction" "fractionId"

modNId :: TestTree
modNId = amonoidIdentity Gens.modN MkEqExact "ModN" "modNId"

modPId :: TestTree
modPId = amonoidIdentity Gens.modN MkEqExact "ModP" "modPId"

nonNegativeId :: TestTree
nonNegativeId = amonoidIdentity Gens.nonNegative MkEqExact "NonNegative" "nonNegativeId"

amonoidIdentity ::
  ( AddConstraint a ~ a,
    AMonoid a,
    Show a
  ) =>
  Gen a ->
  (a -> Equality eq a) ->
  TestName ->
  PropertyName ->
  TestTree
amonoidIdentity = Utils.identity (.+.) zero
