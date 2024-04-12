{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Data.ModP (props) where

import Data.Bounds (AnyUpperBounded)
import Data.Text.Display qualified as D
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import Numeric.Algebra.Additive.AGroup (AGroup ((.-.)))
import Numeric.Algebra.Additive.ASemigroup (ASemigroup ((.+.)))
import Numeric.Algebra.Multiplicative.MGroup (MGroup ((.%.)))
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup ((.*.)))
import Numeric.Data.ModP qualified as ModP
import Numeric.Data.ModP.Internal (ModP (MkModP, UnsafeModP), reallyUnsafeModP)
import Test.Prelude
import Test.TestBounds (TestBounds (maxVal))
import Utils qualified

-- TODO: We would like to have the same tests here as for ModN i.e. test that
-- modular arithmetic works right up to the size limit for each type. Alas,
-- it turns out isPrime's performance is not good and e.g.
-- isPrime 2147483629 takes far too long (so Int32 is out). Thus we must first
-- improve the performance.

props :: TestTree
props =
  testGroup
    "Numeric.Data.ModP"
    [ testUnsafe,
      int8Props,
      int16Props,
      integerProps,
      word8Props,
      word16Props,
      naturalProps,
      elimProps,
      showSpecs,
      displaySpecs
    ]

testUnsafe :: TestTree
testUnsafe = testCase "Test unsafeModP" $ do
  UnsafeModP 5 @=? ModP.unsafeModP @7 @Integer 5

  Utils.assertPureErrorCall expectedEx (ModP.unsafeModP @8 @Integer 5)
  where
    expectedEx = "Numeric.Data.ModP.unsafeModP: Received non-prime: 8"

int8Props :: TestTree
int8Props =
  testGroup
    "Int8"
    [ mkModPInt8,
      mkModPFailLargeInt8,
      mkModPFailCompositeInt8,
      addTotalInt8,
      subTotalInt8,
      multTotalInt8,
      divTotalInt8,
      invertInt8,
      boundedVals @127 @Natural
    ]

mkModPInt8 :: TestTree
mkModPInt8 =
  testPropertyCompat "mkModP x" "mkModNInt8" $
    mkModP' @127 @Int8

mkModPFailLargeInt8 :: TestTree
mkModPFailLargeInt8 =
  testPropertyCompat "mkModP fails too large" "mkModPFailLargeInt8" $
    mkModPFailSize @131 @Int8 "Int8" "127" "131"

mkModPFailCompositeInt8 :: TestTree
mkModPFailCompositeInt8 =
  testPropertyCompat "mkModP fails for non-prime" "mkModPFailCompositeInt8" $
    mkModPFailComposite @126 @Int8 "126"

addTotalInt8 :: TestTree
addTotalInt8 =
  testPropertyCompat "(.+.) implements modular addition" "addTotalInt8" $
    addTotal' @127 @Int8

subTotalInt8 :: TestTree
subTotalInt8 =
  testPropertyCompat "(.-.) implements modular subtraction" "subTotalInt8" $
    subTotal' @127 @Int8

multTotalInt8 :: TestTree
multTotalInt8 =
  testPropertyCompat "(.*.) implements modular multiplication" "multTotalInt8" $
    multTotal' @127 @Int8

divTotalInt8 :: TestTree
divTotalInt8 =
  testPropertyCompat "(.%.) implements modular division" "divTotalInt8" $
    divTotal' @127 @Int8

invertInt8 :: TestTree
invertInt8 =
  testPropertyCompat "1 == x * invert x" "invertInt8" $
    invert' @127 @Int8

int16Props :: TestTree
int16Props =
  testGroup
    "Int16"
    [ mkModPInt16,
      mkModPFailLargeInt16,
      mkModPFailCompositeInt16,
      addTotalInt16,
      subTotalInt16,
      multTotalInt16,
      divTotalInt16,
      invertInt16,
      boundedVals @32749 @Natural
    ]

mkModPInt16 :: TestTree
mkModPInt16 =
  testPropertyCompat "mkModP x" "mkModNInt16" $
    mkModP' @32749 @Int16

mkModPFailLargeInt16 :: TestTree
mkModPFailLargeInt16 =
  testPropertyCompat "mkModP fails too large" "mkModPFailLargeInt16" $
    mkModPFailSize @32771 @Int16 "Int16" "32767" "32771"

mkModPFailCompositeInt16 :: TestTree
mkModPFailCompositeInt16 =
  testPropertyCompat "mkModP fails for non-prime" "mkModPFailCompositeInt16" $
    mkModPFailComposite @32766 @Int16 "32766"

addTotalInt16 :: TestTree
addTotalInt16 =
  testPropertyCompat "(.+.) implements modular addition" "addTotalInt16" $
    addTotal' @32749 @Int16

subTotalInt16 :: TestTree
subTotalInt16 =
  testPropertyCompat "(.-.) implements modular subtraction" "subTotalInt16" $
    subTotal' @32749 @Int16

multTotalInt16 :: TestTree
multTotalInt16 =
  testPropertyCompat "(.*.) implements modular multiplication" "multTotalInt16" $
    multTotal' @32749 @Int16

divTotalInt16 :: TestTree
divTotalInt16 =
  testPropertyCompat "(.%.) implements modular division" "divTotalInt16" $
    divTotal' @32749 @Int16

invertInt16 :: TestTree
invertInt16 =
  testPropertyCompat "1 == x * invert x" "invertInt16" $
    invert' @32749 @Int16

integerProps :: TestTree
integerProps =
  testGroup
    "Integer"
    [ mkModPInteger,
      addTotalInteger,
      subTotalInteger,
      multTotalInteger,
      divTotalInteger,
      invertInteger,
      boundedVals @65537 @Natural
    ]

mkModPInteger :: TestTree
mkModPInteger =
  testPropertyCompat "mkModP x" "mkModNInteger" $
    mkModP' @65537 @Integer

addTotalInteger :: TestTree
addTotalInteger =
  testPropertyCompat "(.+.) implements modular addition" "addTotalInteger" $
    addTotal' @65537 @Integer

subTotalInteger :: TestTree
subTotalInteger =
  testPropertyCompat "(.-.) implements modular subtraction" "subTotalInteger" $
    subTotal' @65537 @Integer

multTotalInteger :: TestTree
multTotalInteger =
  testPropertyCompat "(.*.) implements modular multiplication" "multTotalInteger" $
    multTotal' @65537 @Integer

divTotalInteger :: TestTree
divTotalInteger =
  testPropertyCompat "(.%.) implements modular division" "divTotalInteger" $
    divTotal' @65537 @Integer

invertInteger :: TestTree
invertInteger =
  testPropertyCompat "1 == x * invert x" "invertInteger" $
    invert' @65537 @Integer

word8Props :: TestTree
word8Props =
  testGroup
    "Word8"
    [ mkModPWord8,
      mkModPFailLargeWord8,
      mkModPFailCompositeWord8,
      addTotalWord8,
      subTotalWord8,
      multTotalWord8,
      divTotalWord8,
      invertWord8,
      boundedVals @251 @Natural
    ]

mkModPWord8 :: TestTree
mkModPWord8 =
  testPropertyCompat "mkModP x" "mkModNWord8" $
    mkModP' @251 @Word8

mkModPFailLargeWord8 :: TestTree
mkModPFailLargeWord8 =
  testPropertyCompat "mkModP fails too large" "mkModPFailLargeWord8" $
    mkModPFailSize @257 @Word8 "Word8" "255" "257"

mkModPFailCompositeWord8 :: TestTree
mkModPFailCompositeWord8 =
  testPropertyCompat "mkModP fails for non-prime" "mkModPFailCompositeWord8" $
    mkModPFailComposite @250 @Word8 "250"

addTotalWord8 :: TestTree
addTotalWord8 =
  testPropertyCompat "(.+.) implements modular addition" "addTotalWord8" $
    addTotal' @251 @Word8

subTotalWord8 :: TestTree
subTotalWord8 =
  testPropertyCompat "(.-.) implements modular subtraction" "subTotalWord8" $
    subTotal' @251 @Word8

multTotalWord8 :: TestTree
multTotalWord8 =
  testPropertyCompat "(.*.) implements modular multiplication" "multTotalWord8" $
    multTotal' @251 @Word8

divTotalWord8 :: TestTree
divTotalWord8 =
  testPropertyCompat "(.%.) implements modular division" "divTotalWord8" $
    divTotal' @251 @Word8

invertWord8 :: TestTree
invertWord8 =
  testPropertyCompat "1 == x * invert x" "invertWord8" $
    invert' @251 @Word8

word16Props :: TestTree
word16Props =
  testGroup
    "Word16"
    [ mkModPWord16,
      mkModPFailLargeWord16,
      mkModPFailCompositeWord16,
      addTotalWord16,
      subTotalWord16,
      multTotalWord16,
      divTotalWord16,
      invertWord16,
      boundedVals @65521 @Natural
    ]

mkModPWord16 :: TestTree
mkModPWord16 =
  testPropertyCompat "mkModP x" "mkModNWord16" $
    mkModP' @65521 @Word16

mkModPFailLargeWord16 :: TestTree
mkModPFailLargeWord16 =
  testPropertyCompat "mkModP fails too large" "mkModPFailLargeWord16" $
    mkModPFailSize @65537 @Word16 "Word16" "65535" "65537"

mkModPFailCompositeWord16 :: TestTree
mkModPFailCompositeWord16 =
  testPropertyCompat "mkModP fails for non-prime" "mkModPFailCompositeWord16" $
    mkModPFailComposite @65520 @Word16 "65520"

addTotalWord16 :: TestTree
addTotalWord16 =
  testPropertyCompat "(.+.) implements modular addition" "addTotalWord16" $
    addTotal' @65521 @Word16

subTotalWord16 :: TestTree
subTotalWord16 =
  testPropertyCompat "(.-.) implements modular subtraction" "subTotalWord16" $
    subTotal' @65521 @Word16

multTotalWord16 :: TestTree
multTotalWord16 =
  testPropertyCompat "(.*.) implements modular multiplication" "multTotalWord16" $
    multTotal' @65521 @Word16

divTotalWord16 :: TestTree
divTotalWord16 =
  testPropertyCompat "(.%.) implements modular division" "divTotalWord16" $
    divTotal' @65521 @Word16

invertWord16 :: TestTree
invertWord16 =
  testPropertyCompat "1 == x * invert x" "invertWord16" $
    invert' @65521 @Word16

naturalProps :: TestTree
naturalProps =
  testGroup
    "Natural"
    [ mkModPNatural,
      addTotalNatural,
      subTotalNatural,
      multTotalNatural,
      divTotalNatural,
      invertNatural,
      boundedVals @65537 @Natural
    ]

mkModPNatural :: TestTree
mkModPNatural =
  testPropertyCompat "mkModP x" "mkModPNatural" $
    mkModP' @65537 @Natural

addTotalNatural :: TestTree
addTotalNatural =
  testPropertyCompat "(.+.) implements modular addition" "addTotalNatural" $
    addTotal' @65537 @Natural

subTotalNatural :: TestTree
subTotalNatural =
  testPropertyCompat "(.-.) implements modular subtraction" "subTotalNatural" $
    subTotal' @65537 @Natural

multTotalNatural :: TestTree
multTotalNatural =
  testPropertyCompat "(.*.) implements modular multiplication" "multTotalNatural" $
    multTotal' @65537 @Natural

divTotalNatural :: TestTree
divTotalNatural =
  testPropertyCompat "(.%.) implements modular division" "divTotalNatural" $
    divTotal' @65537 @Natural

invertNatural :: TestTree
invertNatural =
  testPropertyCompat "1 == x * invert x" "invertNatural" $
    invert' @65537 @Natural

addTotal' ::
  forall p a.
  ( ASemigroup (ModP p a),
    Integral a,
    KnownNat p,
    Show a,
    TestBounds a
  ) =>
  Property
addTotal' = property $ do
  mx@(MkModP x) <- forAll (anyNat @p @a)
  my@(MkModP y) <- forAll anyNat
  let mz@(MkModP z) = mx .+. my
      z' = (toInteger x + toInteger y) `mod` p'

  annotateShow mx
  annotateShow my
  annotateShow mz

  z' === toInteger z
  where
    p' = toInteger $ natVal @p Proxy

subTotal' ::
  forall p a.
  ( AGroup (ModP p a),
    Integral a,
    KnownNat p,
    Show a,
    TestBounds a
  ) =>
  Property
subTotal' = property $ do
  mx@(MkModP x) <- forAll (anyNat @p @a)
  my@(MkModP y) <- forAll anyNat
  let mz@(MkModP z) = mx .-. my
      z' = (toInteger x - toInteger y) `mod` p'

  annotateShow mx
  annotateShow my
  annotateShow mz

  z' === toInteger z
  where
    p' = fromIntegral $ natVal @p Proxy

multTotal' ::
  forall p a.
  ( MSemigroup (ModP p a),
    Integral a,
    KnownNat p,
    Show a,
    TestBounds a
  ) =>
  Property
multTotal' = property $ do
  mx@(MkModP x) <- forAll (anyNat @p @a)
  my@(MkModP y) <- forAll anyNat
  let mz@(MkModP z) = mx .*. my
      z' = (toInteger x * toInteger y) `mod` p'

  annotateShow mx
  annotateShow my
  annotateShow mz

  z' === toInteger z
  where
    p' = fromIntegral $ natVal @p Proxy

divTotal' ::
  forall p a.
  ( MGroup (ModP p a),
    Integral a,
    KnownNat p,
    Show a,
    TestBounds a
  ) =>
  Property
divTotal' = property $ do
  mx <- forAll (anyNat @p @a)
  nzy <- forAll (genNZ @p @a)
  let mz = mx .%. nzy
  mx === mz .*. nzy

invert' ::
  forall p a.
  ( MGroup (ModP p a),
    Integral a,
    KnownNat p,
    Show a,
    TestBounds a
  ) =>
  Property
invert' = property $ do
  nz <- forAll (genNZ @p @a)
  let nInv = ModP.invert nz
  reallyUnsafeModP 1 === nz .*. nInv

mkModP' ::
  forall p a.
  ( AnyUpperBounded a,
    Integral a,
    KnownNat p,
    Show a,
    TestBounds a,
    Typeable a
  ) =>
  Property
mkModP' = property $ do
  x <- forAll (nonneg @a)
  let mx@(MkModP x') = ModP.unsafeModP @p x

  annotateShow mx

  x `mod` p' === x'
  where
    p' = fromIntegral $ natVal @p Proxy

mkModPFailSize ::
  forall p a.
  ( AnyUpperBounded a,
    Integral a,
    KnownNat p,
    Show a,
    TestBounds a,
    Typeable a
  ) =>
  String ->
  String ->
  String ->
  Property
mkModPFailSize tyStr maxStr modStr = property $ do
  x <- forAll (nonneg @a)
  case ModP.mkModP @p x of
    Left s -> msg === s
    Right y -> do
      annotate ("Expected failure, received: " ++ show y)
      failure
  where
    msg =
      mconcat
        [ "Type '",
          tyStr,
          "' has a maximum size of ",
          maxStr,
          ". This is not large enough to safely implement mod ",
          modStr,
          "."
        ]

mkModPFailComposite ::
  forall p a.
  ( AnyUpperBounded a,
    Integral a,
    KnownNat p,
    Show a,
    TestBounds a,
    Typeable a
  ) =>
  String ->
  Property
mkModPFailComposite primeStr = property $ do
  x <- forAll (nonneg @a)
  case ModP.mkModP @p x of
    Left s -> msg === s
    Right y -> do
      annotate ("Expected failure, received: " ++ show y)
      failure
  where
    msg =
      mconcat
        [ "Received non-prime: ",
          primeStr
        ]

boundedVals ::
  forall p a.
  ( AnyUpperBounded a,
    Integral a,
    KnownNat p,
    Show a,
    Typeable a
  ) =>
  TestTree
boundedVals = testCase "Min/max bounds" $ do
  0 @=? ModP.unModP (minBound @(ModP p a))

  nTerm - 1 @=? ModP.unModP (maxBound @(ModP p a))
  where
    nTerm = fromIntegral $ natVal @p Proxy

genNZ ::
  forall p a.
  ( Integral a,
    KnownNat p,
    TestBounds a
  ) =>
  Gen (ModP p a)
genNZ = do
  x <- HG.filter (\x' -> x' `mod` p' /= 0) $ HG.integral $ HR.exponential 2 maxVal
  let y = reallyUnsafeModP @p x
  pure y
  where
    p' = fromIntegral $ natVal @p Proxy

anyNat :: forall p a. (Integral a, KnownNat p, TestBounds a) => Gen (ModP p a)
anyNat = reallyUnsafeModP <$> HG.integral (HR.exponentialFrom 0 0 maxVal)

nonneg :: forall a. (Integral a, TestBounds a) => Gen a
nonneg = HG.integral $ HR.exponentialFrom 1 1 maxVal

elimProps :: TestTree
elimProps =
  testPropertyCompat desc "elimProps" $
    property $ do
      mp@(MkModP n) <- forAll (anyNat @350 @Int)

      n === ModP.unModP mp
      n === mp.unModP
      n === view #unModP mp
      n === view ModP._MkModP mp
  where
    desc = "elim (MkModP x) === x"

showSpecs :: TestTree
showSpecs = testCase "Shows ModP" $ do
  "MkModP 2 (mod 7)" @=? show (ModP.unsafeModP @7 @Integer 2)
  "MkModP 9 (mod 13)" @=? show (ModP.unsafeModP @13 @Integer 22)

displaySpecs :: TestTree
displaySpecs = testCase "Displays ModP" $ do
  "2 (mod 7)" @=? D.display (ModP.unsafeModP @7 @Integer 2)
  "9 (mod 13)" @=? D.display (ModP.unsafeModP @13 @Integer 22)
