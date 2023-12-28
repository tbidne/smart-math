{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Data.ModN (props) where

import Data.Bounds (UpperBoundless)
import Data.Text.Display qualified as D
import GHC.Natural (Natural)
import Hedgehog (MonadGen, Property, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import Numeric.Algebra.Additive.AGroup (AGroup ((.-.)))
import Numeric.Algebra.Additive.ASemigroup (ASemigroup ((.+.)))
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup ((.*.)))
import Numeric.Data.ModN (ModN (MkModN))
import Numeric.Data.ModN qualified as ModN
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.HUnit ((@=?))
import Test.Tasty.HUnit qualified as HUnit
import Test.TestBounds (TestBounds (maxVal))
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Numeric.Data.ModN"
    [ intProps,
      natProps,
      showSpecs,
      displaySpecs
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
mkModNInt =
  Utils.testPropertyCompat "mkModN x" "mkModNInt" $
    mkModN' @Natural

addTotalInt :: TestTree
addTotalInt =
  Utils.testPropertyCompat "(.+.) implements modular addition over Integers" "addTotalInt" $
    addTotal' @Natural

subTotalInt :: TestTree
subTotalInt =
  Utils.testPropertyCompat "(.-.) implements modular subtraction over Integers" "subTotalInt" $
    subTotal' @Natural

multTotalInt :: TestTree
multTotalInt =
  Utils.testPropertyCompat "(.*.) implements modular multiplication over Integers" "multTotalInt" $
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
mkModNNat =
  Utils.testPropertyCompat "mkModN x" "mkModNNat" $
    mkModN' @Natural

addTotalNat :: TestTree
addTotalNat =
  Utils.testPropertyCompat "(.+.) implements modular addition over Naturals" "addTotalNat" $
    addTotal' @Natural

subTotalNat :: TestTree
subTotalNat =
  Utils.testPropertyCompat "(.-.) implements modular subtraction over Naturals" "subTotalNat" $
    subTotal' @Natural

multTotalNat :: TestTree
multTotalNat =
  Utils.testPropertyCompat "(.*.) implements modular multiplication over Naturals" "multTotalNat" $
    multTotal' @Natural

mkModN' :: forall a. (Integral a, Show a, TestBounds a, UpperBoundless a) => Property
mkModN' = H.property $ do
  x <- H.forAll (nonneg @a)
  let MkModN x' = ModN.mkModN @12 x
  x `mod` 12 === x'

addTotal' ::
  forall a.
  ( ASemigroup (ModN 65536 a),
    Integral a,
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
  ( AGroup (ModN 65536 a),
    Integral a,
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
  ( Integral a,
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

anyNat :: forall a m. (Integral a, MonadGen m, TestBounds a, UpperBoundless a) => m (ModN 65536 a)
anyNat = ModN.mkModN <$> HG.integral (HR.exponentialFrom 0 0 maxVal)

showSpecs :: TestTree
showSpecs = HUnit.testCase "Shows ModN" $ do
  "MkModN 2 (mod 8)" @=? show (ModN.mkModN @8 @Integer 2)
  "MkModN 10 (mod 12)" @=? show (ModN.mkModN @12 @Integer 22)

displaySpecs :: TestTree
displaySpecs = HUnit.testCase "Displays ModN" $ do
  "2 (mod 8)" @=? D.display (ModN.mkModN @8 @Integer 2)
  "10 (mod 12)" @=? D.display (ModN.mkModN @12 @Integer 22)
