{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Data.ModN (props) where

import GHC.Natural (Natural)
import Hedgehog (MonadGen, Property, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import MaxRuns (MaxRuns (..))
import Numeric.Algebra.Additive.AGroup (AGroup (..), SubtractConstraint)
import Numeric.Algebra.Additive.ASemigroup (ASemigroup (..))
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup (..))
import Numeric.Class.Boundless (UpperBoundless)
import Numeric.Data.ModN (ModN (..))
import Numeric.Data.ModN qualified as ModN
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.TestBounds (TestBounds (..))
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Numeric.Data.ModN"
    [ intProps,
      natProps
    ]

intProps :: TestTree
intProps =
  T.testGroup
    "Integer"
    [ mkModNInt,
      addTotalInt,
      subTotalInt,
      multTotalInt
    ]

mkModNInt :: TestTree
mkModNInt = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "mkModN x" "mkModNInt" $
    H.withTests limit $
      mkModN' @Natural

addTotalInt :: TestTree
addTotalInt = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "(.+.) implements modular addition over Integers" "addTotalInt" $
    H.withTests limit $
      addTotal' @Natural

subTotalInt :: TestTree
subTotalInt = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "(.-.) implements modular subtraction over Integers" "subTotalInt" $
    H.withTests limit $
      subTotal' @Natural

multTotalInt :: TestTree
multTotalInt = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "(.*.) implements modular multiplication over Integers" "multTotalInt" $
    H.withTests limit $
      multTotal' @Natural

natProps :: TestTree
natProps =
  T.testGroup
    "Natural"
    [ mkModNNat,
      addTotalNat,
      subTotalNat,
      multTotalNat
    ]

mkModNNat :: TestTree
mkModNNat = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "mkModN x" "mkModNNat" $
    H.withTests limit $
      mkModN' @Natural

addTotalNat :: TestTree
addTotalNat = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "(.+.) implements modular addition over Naturals" "addTotalNat" $
    H.withTests limit $
      addTotal' @Natural

subTotalNat :: TestTree
subTotalNat = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "(.-.) implements modular subtraction over Naturals" "subTotalNat" $
    H.withTests limit $
      subTotal' @Natural

multTotalNat :: TestTree
multTotalNat = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "(.*.) implements modular multiplication over Naturals" "multTotalNat" $
    H.withTests limit $
      multTotal' @Natural

mkModN' :: forall a. (Show a, TestBounds a, UpperBoundless a) => Property
mkModN' = H.property $ do
  x <- H.forAll (nonneg @a)
  let MkModN x' = ModN.mkModN @12 x
  x `mod` 12 === x'

addTotal' ::
  forall a.
  ( AddConstraint (ModN 65536 a) ~ ModN 65536 a,
    ASemigroup (ModN 65536 a),
    Show a,
    TestBounds a,
    UpperBoundless a
  ) =>
  Property
addTotal' = H.property $ do
  mx@(MkModN x) <- H.forAll (anyNat @a)
  my@(MkModN y) <- H.forAll anyNat
  let MkModN mz = mx .+. my
      z = (x + y) `mod` 65536
  z === mz

subTotal' ::
  forall a.
  ( SubtractConstraint (ModN 65536 a) ~ ModN 65536 a,
    AGroup (ModN 65536 a),
    Show a,
    TestBounds a,
    UpperBoundless a
  ) =>
  Property
subTotal' = H.property $ do
  mx@(MkModN x) <- H.forAll (anyNat @a)
  my@(MkModN y) <- H.forAll anyNat
  let MkModN mz = mx .-. my
      z = (65536 - y + x) `mod` 65536
  z === mz

multTotal' ::
  forall a.
  ( MultConstraint (ModN 65536 a) ~ ModN 65536 a,
    MSemigroup (ModN 65536 a),
    Show a,
    TestBounds a,
    UpperBoundless a
  ) =>
  Property
multTotal' = H.property $ do
  mx@(MkModN x) <- H.forAll (anyNat @a)
  my@(MkModN y) <- H.forAll anyNat
  let MkModN mz = mx .*. my
      z = (x * y) `mod` 65536
  z === mz

nonneg :: forall a m. (Integral a, MonadGen m, TestBounds a) => m a
nonneg = HG.integral $ HR.exponentialFrom 20 20 maxVal

anyNat :: forall a m. (MonadGen m, TestBounds a, UpperBoundless a) => m (ModN 65536 a)
anyNat = ModN.mkModN <$> HG.integral (HR.exponentialFrom 0 0 maxVal)
