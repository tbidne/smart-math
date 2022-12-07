{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Data.ModP (props) where

import Data.Bounds (UpperBoundless)
import Data.Functor.Identity (Identity)
import GHC.Natural (Natural)
import Gens qualified
import Hedgehog (GenBase, MonadGen, Property, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import Numeric.Algebra.Additive.AGroup (AGroup (..))
import Numeric.Algebra.Additive.AMonoid (AMonoid)
import Numeric.Algebra.Additive.ASemigroup (ASemigroup (..))
import Numeric.Algebra.Multiplicative.MGroup (MGroup (..), unsafeAMonoidNonZero)
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup (..))
import Numeric.Data.ModP (ModP (..), reallyUnsafeModP)
import Numeric.Data.ModP qualified as ModP
import Numeric.Data.NonZero (NonZero (..))
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.TestBounds (TestBounds (..))
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Numeric.Data.ModP"
    [ mkModPSucceed,
      mkModPFail,
      intProps,
      natProps
    ]

mkModPSucceed :: TestTree
mkModPSucceed =
  Utils.testPropertyCompat "mkModP x succeeds for prime" "mkModPSucceed" $
    H.property $ do
      x <- H.forAll Gens.natural
      case ModP.mkModP @65537 x of
        Nothing -> H.failure
        Just (MkModP x') -> x `mod` 65537 === x'

mkModPFail :: TestTree
mkModPFail =
  Utils.testPropertyCompat "mkModP x fails for non-prime" "mkModPFail" $
    H.property $ do
      x <- H.forAll Gens.natural
      Nothing === ModP.mkModP @65536 x

intProps :: TestTree
intProps =
  T.testGroup
    "Integer"
    [ addTotalInt,
      subTotalInt,
      multTotalInt,
      divTotalInt,
      invertInt
    ]

addTotalInt :: TestTree
addTotalInt =
  Utils.testPropertyCompat "(.+.) implements modular addition over Integers" "addTotalInt" $
    addTotal' @Integer

subTotalInt :: TestTree
subTotalInt =
  Utils.testPropertyCompat "(.-.) implements modular subtraction over Integers" "subTotalInt" $
    subTotal' @Integer

multTotalInt :: TestTree
multTotalInt =
  Utils.testPropertyCompat "(.*.) implements modular multiplication over Integers" "multTotalInt" $
    multTotal' @Integer

divTotalInt :: TestTree
divTotalInt =
  Utils.testPropertyCompat "(.%.) implements modular division over Integers" "divTotalInt" $
    divTotal' @Integer

invertInt :: TestTree
invertInt =
  Utils.testPropertyCompat "1 == x * invert x over Integers" "invertInt" $
    invert' @Integer

natProps :: TestTree
natProps =
  T.testGroup
    "Natural"
    [ addTotalNat,
      subTotalNat,
      multTotalNat,
      divTotalNat,
      invertNat
    ]

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

divTotalNat :: TestTree
divTotalNat =
  Utils.testPropertyCompat "(.%.) implements modular division over Naturals" "divTotalNat" $
    divTotal' @Natural

invertNat :: TestTree
invertNat =
  Utils.testPropertyCompat "1 == x * invert x over Naturals" "invertNat" $
    invert' @Natural

addTotal' ::
  forall a.
  ( ASemigroup (ModP 65537 a),
    Integral a,
    Show a,
    TestBounds a,
    UpperBoundless a
  ) =>
  Property
addTotal' = H.property $ do
  mx@(MkModP x) <- H.forAll (anyNat @a)
  my@(MkModP y) <- H.forAll anyNat
  let MkModP mz = mx .+. my
      z = (x + y) `mod` 65537
  z === mz

subTotal' ::
  forall a.
  ( AGroup (ModP 65537 a),
    Integral a,
    Show a,
    TestBounds a,
    UpperBoundless a
  ) =>
  Property
subTotal' = H.property $ do
  mx@(MkModP x) <- H.forAll (anyNat @a)
  my@(MkModP y) <- H.forAll anyNat
  let MkModP mz = mx .-. my
      z = (65537 - y + x) `mod` 65537
  z === mz

multTotal' ::
  forall a.
  ( Integral a,
    MSemigroup (ModP 65537 a),
    Show a,
    TestBounds a,
    UpperBoundless a
  ) =>
  Property
multTotal' = H.property $ do
  mx@(MkModP x) <- H.forAll (anyNat @a)
  my@(MkModP y) <- H.forAll anyNat
  let MkModP mz = mx .*. my
      z = (x * y) `mod` 65537
  z === mz

divTotal' ::
  forall a.
  ( AMonoid (ModP 65537 a),
    Integral a,
    MGroup (ModP 65537 a),
    Show a,
    TestBounds a,
    UpperBoundless a
  ) =>
  Property
divTotal' = H.property $ do
  mx <- H.forAll (anyNat @a)
  nzy@(MkNonZero my) <- H.forAll (genNZ @a)
  let mz = mx .%. nzy
  mx === mz .*. my

invert' ::
  forall a.
  ( AMonoid (ModP 65537 a),
    Integral a,
    MGroup (ModP 65537 a),
    Show a,
    TestBounds a,
    UpperBoundless a
  ) =>
  Property
invert' = H.property $ do
  nz@(MkNonZero n) <- H.forAll (genNZ @a)
  let nInv = ModP.invert nz
  reallyUnsafeModP 1 === n .*. nInv

genNZ :: forall a m. (AMonoid (ModP 65537 a), GenBase m ~ Identity, Integral a, MonadGen m, TestBounds a, UpperBoundless a) => m (NonZero (ModP 65537 a))
genNZ = do
  x <- HG.filter (\x' -> x' `mod` 65537 /= 0) $ HG.integral $ HR.exponential 2 maxVal
  let y = unsafeAMonoidNonZero $ reallyUnsafeModP @65537 x
  pure y

anyNat :: forall a m. (Integral a, MonadGen m, TestBounds a, UpperBoundless a) => m (ModP 65537 a)
anyNat = reallyUnsafeModP <$> HG.integral (HR.exponentialFrom 0 0 maxVal)
