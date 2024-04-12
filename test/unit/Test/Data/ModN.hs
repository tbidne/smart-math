{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Data.ModN (props) where

import Data.Bounds (AnyUpperBounded)
import Data.Text.Display qualified as D
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import Numeric.Algebra.Additive.AGroup (AGroup ((.-.)))
import Numeric.Algebra.Additive.ASemigroup (ASemigroup ((.+.)))
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup ((.*.)))
import Numeric.Data.ModN qualified as ModN
import Numeric.Data.ModN.Internal (ModN (MkModN))
import Test.Prelude
import Test.TestBounds (TestBounds (maxVal))

props :: TestTree
props =
  testGroup
    "Numeric.Data.ModN"
    [ int8Props,
      int16Props,
      int32Props,
      int64Props,
      intProps,
      integerProps,
      word8Props,
      word16Props,
      word32Props,
      word64Props,
      wordProps,
      naturalProps,
      elimProps,
      showSpecs,
      displaySpecs
    ]

int8Props :: TestTree
int8Props =
  testGroup
    "Int8"
    [ mkModNInt8,
      mkModNFailureInt8,
      addTotalInt8,
      subTotalInt8,
      multTotalInt8,
      boundedVals @127 @Int8
    ]

mkModNInt8 :: TestTree
mkModNInt8 =
  testPropertyCompat "mkModN x" "mkModNInt8" $
    mkModN' @127 @Int8

mkModNFailureInt8 :: TestTree
mkModNFailureInt8 =
  testPropertyCompat "mkModN fails" "mkModNFailureInt8" $
    mkModNFailure @128 @Int8 "Int8" "127" "128"

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

int16Props :: TestTree
int16Props =
  testGroup
    "Int16"
    [ mkModNInt16,
      mkModNFailureInt16,
      addTotalInt16,
      subTotalInt16,
      multTotalInt16,
      boundedVals @32767 @Int16
    ]

mkModNInt16 :: TestTree
mkModNInt16 =
  testPropertyCompat "mkModN x" "mkModNInt16" $
    mkModN' @32767 @Int16

mkModNFailureInt16 :: TestTree
mkModNFailureInt16 =
  testPropertyCompat "mkModN fails" "mkModNFailureInt16" $
    mkModNFailure @32768 @Int16 "Int16" "32767" "32768"

addTotalInt16 :: TestTree
addTotalInt16 =
  testPropertyCompat "(.+.) implements modular addition" "addTotalInt16" $
    addTotal' @32767 @Int16

subTotalInt16 :: TestTree
subTotalInt16 =
  testPropertyCompat "(.-.) implements modular subtraction" "subTotalInt16" $
    subTotal' @32767 @Int16

multTotalInt16 :: TestTree
multTotalInt16 =
  testPropertyCompat "(.*.) implements modular multiplication" "multTotalInt16" $
    multTotal' @32767 @Int16

int32Props :: TestTree
int32Props =
  testGroup
    "Int32"
    [ mkModNInt32,
      mkModNFailureInt32,
      addTotalInt32,
      subTotalInt32,
      multTotalInt32,
      boundedVals @2147483647 @Int32
    ]

mkModNInt32 :: TestTree
mkModNInt32 =
  testPropertyCompat "mkModN x" "mkModNInt32" $
    mkModN' @2147483647 @Int32

mkModNFailureInt32 :: TestTree
mkModNFailureInt32 =
  testPropertyCompat "mkModN fails" "mkModNFailureInt32" $
    mkModNFailure @2147483648 @Int32 "Int32" "2147483647" "2147483648"

addTotalInt32 :: TestTree
addTotalInt32 =
  testPropertyCompat "(.+.) implements modular addition" "addTotalInt32" $
    addTotal' @2147483647 @Int32

subTotalInt32 :: TestTree
subTotalInt32 =
  testPropertyCompat "(.-.) implements modular subtraction" "subTotalInt32" $
    subTotal' @2147483647 @Int32

multTotalInt32 :: TestTree
multTotalInt32 =
  testPropertyCompat "(.*.) implements modular multiplication" "multTotalInt32" $
    multTotal' @2147483647 @Int32

int64Props :: TestTree
int64Props =
  testGroup
    "Int64"
    [ mkModNInt64,
      mkModNFailureInt64,
      addTotalInt64,
      subTotalInt64,
      multTotalInt64,
      boundedVals @9223372036854775807 @Int64
    ]

mkModNInt64 :: TestTree
mkModNInt64 =
  testPropertyCompat "mkModN x" "mkModNInt64" $
    mkModN' @9223372036854775807 @Int64

mkModNFailureInt64 :: TestTree
mkModNFailureInt64 =
  testPropertyCompat "mkModN fails" "mkModNFailureInt64" $
    mkModNFailure @9223372036854775808 @Int64 "Int64" "9223372036854775807" "9223372036854775808"

addTotalInt64 :: TestTree
addTotalInt64 =
  testPropertyCompat "(.+.) implements modular addition" "addTotalInt64" $
    addTotal' @9223372036854775807 @Int64

subTotalInt64 :: TestTree
subTotalInt64 =
  testPropertyCompat "(.-.) implements modular subtraction" "subTotalInt64" $
    subTotal' @9223372036854775807 @Int64

multTotalInt64 :: TestTree
multTotalInt64 =
  testPropertyCompat "(.*.) implements modular multiplication" "multTotalInt64" $
    multTotal' @9223372036854775807 @Int64

intProps :: TestTree
intProps =
  testGroup
    "Int"
    [ mkModNInt,
      mkModNFailureInt,
      addTotalInt,
      subTotalInt,
      multTotalInt,
      boundedVals @65536 @Int
    ]

mkModNInt :: TestTree
mkModNInt =
  testPropertyCompat "mkModN x" "mkModNInt" $
    mkModN' @12 @Int

mkModNFailureInt :: TestTree
mkModNFailureInt =
  testPropertyCompat "mkModN fails" "mkModNFailureInt" $
    mkModNFailure @9223372036854775808 @Int "Int" "9223372036854775807" "9223372036854775808"

addTotalInt :: TestTree
addTotalInt =
  testPropertyCompat "(.+.) implements modular addition" "addTotalInt" $
    addTotal' @65536 @Int

subTotalInt :: TestTree
subTotalInt =
  testPropertyCompat "(.-.) implements modular subtraction" "subTotalInt" $
    subTotal' @65536 @Int

multTotalInt :: TestTree
multTotalInt =
  testPropertyCompat "(.*.) implements modular multiplication" "multTotalInt" $
    multTotal' @65536 @Int

integerProps :: TestTree
integerProps =
  testGroup
    "Integer"
    [ mkModNInteger,
      addTotalInteger,
      subTotalInteger,
      multTotalInteger,
      boundedVals @65536 @Integer
    ]

mkModNInteger :: TestTree
mkModNInteger =
  testPropertyCompat "mkModN x" "mkModNInteger" $
    mkModN' @12 @Integer

addTotalInteger :: TestTree
addTotalInteger =
  testPropertyCompat "(.+.) implements modular addition" "addTotalInteger" $
    addTotal' @65536 @Integer

subTotalInteger :: TestTree
subTotalInteger =
  testPropertyCompat "(.-.) implements modular subtraction" "subTotalInteger" $
    subTotal' @65536 @Integer

multTotalInteger :: TestTree
multTotalInteger =
  testPropertyCompat "(.*.) implements modular multiplication" "multTotalInteger" $
    multTotal' @65536 @Integer

word8Props :: TestTree
word8Props =
  testGroup
    "Word8"
    [ mkModNWord8,
      mkModNFailureWord8,
      addTotalWord8,
      subTotalWord8,
      multTotalWord8,
      boundedVals @255 @Word8
    ]

mkModNWord8 :: TestTree
mkModNWord8 =
  testPropertyCompat "mkModN x" "mkModNWord8" $
    mkModN' @255 @Word8

mkModNFailureWord8 :: TestTree
mkModNFailureWord8 =
  testPropertyCompat "mkModN fails" "mkModNFailureWord8" $
    mkModNFailure @256 @Word8 "Word8" "255" "256"

addTotalWord8 :: TestTree
addTotalWord8 =
  testPropertyCompat "(.+.) implements modular addition" "addTotalWord8" $
    addTotal' @255 @Word8

subTotalWord8 :: TestTree
subTotalWord8 =
  testPropertyCompat "(.-.) implements modular subtraction" "subTotalWord8" $
    subTotal' @255 @Word8

multTotalWord8 :: TestTree
multTotalWord8 =
  testPropertyCompat "(.*.) implements modular multiplication" "multTotalWord8" $
    multTotal' @255 @Word8

word16Props :: TestTree
word16Props =
  testGroup
    "Word16"
    [ mkModNWord16,
      mkModNFailureWord16,
      addTotalWord16,
      subTotalWord16,
      multTotalWord16,
      boundedVals @65535 @Word16
    ]

mkModNWord16 :: TestTree
mkModNWord16 =
  testPropertyCompat "mkModN x" "mkModNWord16" $
    mkModN' @65535 @Word16

mkModNFailureWord16 :: TestTree
mkModNFailureWord16 =
  testPropertyCompat "mkModN fails" "mkModNFailureWord16" $
    mkModNFailure @65536 @Word16 "Word16" "65535" "65536"

addTotalWord16 :: TestTree
addTotalWord16 =
  testPropertyCompat "(.+.) implements modular addition" "addTotalWord16" $
    addTotal' @65535 @Word16

subTotalWord16 :: TestTree
subTotalWord16 =
  testPropertyCompat "(.-.) implements modular subtraction" "subTotalWord16" $
    subTotal' @65535 @Word16

multTotalWord16 :: TestTree
multTotalWord16 =
  testPropertyCompat "(.*.) implements modular multiplication" "multTotalWord16" $
    multTotal' @65535 @Word16

word32Props :: TestTree
word32Props =
  testGroup
    "Word32"
    [ mkModNWord32,
      mkModNFailureWord32,
      addTotalWord32,
      subTotalWord32,
      multTotalWord32,
      boundedVals @4294967295 @Word32
    ]

mkModNWord32 :: TestTree
mkModNWord32 =
  testPropertyCompat "mkModN x" "mkModNWord32" $
    mkModN' @4294967295 @Word32

mkModNFailureWord32 :: TestTree
mkModNFailureWord32 =
  testPropertyCompat "mkModN fails" "mkModNFailureWord32" $
    mkModNFailure @4294967296 @Word32 "Word32" "4294967295" "4294967296"

addTotalWord32 :: TestTree
addTotalWord32 =
  testPropertyCompat "(.+.) implements modular addition" "addTotalWord32" $
    addTotal' @4294967295 @Word32

subTotalWord32 :: TestTree
subTotalWord32 =
  testPropertyCompat "(.-.) implements modular subtraction" "subTotalWord32" $
    subTotal' @4294967295 @Word32

multTotalWord32 :: TestTree
multTotalWord32 =
  testPropertyCompat "(.*.) implements modular multiplication" "multTotalWord32" $
    multTotal' @4294967295 @Word32

word64Props :: TestTree
word64Props =
  testGroup
    "Word64"
    [ mkModNWord64,
      mkModNFailureWord64,
      addTotalWord64,
      subTotalWord64,
      multTotalWord64,
      boundedVals @18446744073709551615 @Word64
    ]

mkModNWord64 :: TestTree
mkModNWord64 =
  testPropertyCompat "mkModN x" "mkModNWord64" $
    mkModN' @18446744073709551615 @Word64

mkModNFailureWord64 :: TestTree
mkModNFailureWord64 =
  testPropertyCompat "mkModN fails" "mkModNFailureWord64" $
    mkModNFailure @18446744073709551616 @Word64 "Word64" "18446744073709551615" "18446744073709551616"

addTotalWord64 :: TestTree
addTotalWord64 =
  testPropertyCompat "(.+.) implements modular addition" "addTotalWord64" $
    addTotal' @18446744073709551615 @Word64

subTotalWord64 :: TestTree
subTotalWord64 =
  testPropertyCompat "(.-.) implements modular subtraction" "subTotalWord64" $
    subTotal' @18446744073709551615 @Word64

multTotalWord64 :: TestTree
multTotalWord64 =
  testPropertyCompat "(.*.) implements modular multiplication" "multTotalWord64" $
    multTotal' @18446744073709551615 @Word64

wordProps :: TestTree
wordProps =
  testGroup
    "Word"
    [ mkModNWord,
      mkModNFailureWord,
      addTotalWord,
      subTotalWord,
      multTotalWord,
      boundedVals @65536 @Word
    ]

mkModNWord :: TestTree
mkModNWord =
  testPropertyCompat "mkModN x" "mkModNWord" $
    mkModN' @12 @Word

mkModNFailureWord :: TestTree
mkModNFailureWord =
  testPropertyCompat "mkModN fails" "mkModNFailureWord" $
    mkModNFailure @18446744073709551616 @Word "Word" "18446744073709551615" "18446744073709551616"

addTotalWord :: TestTree
addTotalWord =
  testPropertyCompat "(.+.) implements modular addition" "addTotalWord" $
    addTotal' @65536 @Word

subTotalWord :: TestTree
subTotalWord =
  testPropertyCompat "(.-.) implements modular subtraction" "subTotalWord" $
    subTotal' @65536 @Word

multTotalWord :: TestTree
multTotalWord =
  testPropertyCompat "(.*.) implements modular multiplication" "multTotalWord" $
    multTotal' @65536 @Word

naturalProps :: TestTree
naturalProps =
  testGroup
    "Natural"
    [ mkModNNatural,
      addTotalNatural,
      subTotalNatural,
      multTotalNatural,
      boundedVals @65536 @Natural
    ]

mkModNNatural :: TestTree
mkModNNatural =
  testPropertyCompat "mkModN x" "mkModNNat" $
    mkModN' @12 @Natural

addTotalNatural :: TestTree
addTotalNatural =
  testPropertyCompat "(.+.) implements modular addition" "addTotalNatural" $
    addTotal' @65536 @Natural

subTotalNatural :: TestTree
subTotalNatural =
  testPropertyCompat "(.-.) implements modular subtraction" "subTotalNatural" $
    subTotal' @65536 @Natural

multTotalNatural :: TestTree
multTotalNatural =
  testPropertyCompat "(.*.) implements modular multiplication" "multTotalNatural" $
    multTotal' @65536 @Natural

mkModN' ::
  forall n a.
  ( AnyUpperBounded a,
    Integral a,
    KnownNat n,
    Show a,
    TestBounds a,
    Typeable a
  ) =>
  Property
mkModN' = property $ do
  x <- forAll (nonneg @a)
  let mx@(MkModN x') = ModN.unsafeModN @n x

  annotateShow mx

  x `mod` n' === x'
  where
    n' = fromIntegral $ natVal @n Proxy

mkModNFailure ::
  forall n a.
  ( AnyUpperBounded a,
    Integral a,
    KnownNat n,
    Show a,
    TestBounds a,
    Typeable a
  ) =>
  String ->
  String ->
  String ->
  Property
mkModNFailure tyStr maxStr modStr = property $ do
  x <- forAll (nonneg @a)
  case ModN.mkModN @n x of
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

boundedVals ::
  forall n a.
  ( AnyUpperBounded a,
    Integral a,
    KnownNat n,
    Show a,
    Typeable a
  ) =>
  TestTree
boundedVals = testCase "Min/max bounds" $ do
  0 @=? ModN.unModN (minBound @(ModN n a))

  nTerm - 1 @=? ModN.unModN (maxBound @(ModN n a))
  where
    nTerm = fromIntegral $ natVal @n Proxy

addTotal' ::
  forall n a.
  ( AnyUpperBounded a,
    ASemigroup (ModN n a),
    Integral a,
    KnownNat n,
    Show a,
    TestBounds a,
    Typeable a
  ) =>
  Property
addTotal' = property $ do
  mx@(MkModN x) <- forAll (anyNat @n @a)
  my@(MkModN y) <- forAll anyNat

  let mz@(MkModN z) = mx .+. my
      z' = (toInteger x + toInteger y) `mod` n'

  annotateShow mx
  annotateShow my
  annotateShow mz

  z' === toInteger z
  where
    n' = toInteger $ natVal @n Proxy

subTotal' ::
  forall n a.
  ( AGroup (ModN n a),
    AnyUpperBounded a,
    Integral a,
    KnownNat n,
    Show a,
    TestBounds a,
    Typeable a
  ) =>
  Property
subTotal' = property $ do
  mx@(MkModN x) <- forAll (anyNat @n @a)
  my@(MkModN y) <- forAll anyNat
  let mz@(MkModN z) = mx .-. my
      z' = (toInteger x - toInteger y) `mod` n'

  annotateShow mx
  annotateShow my
  annotateShow mz

  z' === toInteger z
  where
    n' = fromIntegral $ natVal @n Proxy

multTotal' ::
  forall n a.
  ( AnyUpperBounded a,
    Integral a,
    KnownNat n,
    MSemigroup (ModN n a),
    Show a,
    TestBounds a,
    Typeable a
  ) =>
  Property
multTotal' = property $ do
  mx@(MkModN x) <- forAll (anyNat @n @a)
  my@(MkModN y) <- forAll anyNat
  let mz@(MkModN z) = mx .*. my
      z' = (toInteger x * toInteger y) `mod` n'

  annotateShow mx
  annotateShow my
  annotateShow mz

  z' === toInteger z
  where
    n' = fromIntegral $ natVal @n Proxy

nonneg :: forall a. (Integral a, TestBounds a) => Gen a
nonneg = HG.integral $ HR.exponentialFrom 20 20 maxVal

anyNat ::
  forall n a.
  ( AnyUpperBounded a,
    Integral a,
    KnownNat n,
    TestBounds a,
    Typeable a
  ) =>
  Gen (ModN n a)
anyNat = ModN.unsafeModN <$> HG.integral (HR.exponentialFrom 0 0 maxVal)

elimProps :: TestTree
elimProps =
  testPropertyCompat desc "elimProps" $
    property $ do
      mn@(MkModN n) <- forAll (anyNat @350 @Int)

      n === ModN.unModN mn
      n === mn.unModN
      n === view #unModN mn
      n === view ModN._MkModN mn
  where
    desc = "elim (MkModN x) === x"

showSpecs :: TestTree
showSpecs = testCase "Shows ModN" $ do
  "MkModN 2 (mod 8)" @=? show (ModN.unsafeModN @8 @Integer 2)
  "MkModN 10 (mod 12)" @=? show (ModN.unsafeModN @12 @Integer 22)

displaySpecs :: TestTree
displaySpecs = testCase "Displays ModN" $ do
  "2 (mod 8)" @=? D.display (ModN.unsafeModN @8 @Integer 2)
  "10 (mod 12)" @=? D.display (ModN.unsafeModN @12 @Integer 22)
