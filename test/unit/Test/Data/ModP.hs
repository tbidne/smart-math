{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}

module Test.Data.ModP (props) where

import Data.Bounds (MaybeUpperBounded)
import Data.Text.Display qualified as D
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import Numeric.Algebra
  ( AGroup ((.-.)),
    AMonoid,
    ASemigroup ((.+.)),
    MEuclidean,
    MGroup ((.%.)),
    MSemigroup ((.*.)),
  )
import Numeric.Convert.Integer (FromInteger, ToInteger)
import Numeric.Data.ModP (ModP (MkModP))
import Numeric.Data.ModP qualified as ModP
import Numeric.Data.ModP.Internal qualified as ModPI
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
    tests
  where
    tests =
      testUnsafe
        : intTypeProps
        ++ wordTypeProps
        ++ [ elimProps,
             showSpecs,
             displaySpecs
           ]

    intTypeProps =
      [ int8Props,
        int16Props
      ]
        ++ extraIntProps
        ++ [integerProps]

    wordTypeProps =
      [ word8Props,
        word16Props
      ]
        ++ extraWordProps
        ++ [naturalProps]

testUnsafe :: TestTree
testUnsafe = testCase "Test unsafeModP" $ do
  ModPI.UnsafeModP 5 @=? ModP.unsafeModP @7 @Integer 5
  Utils.assertPureErrorCall expectedEx (ModP.unsafeModP @8 @Integer 5)
  where
    expectedEx = "Numeric.Data.ModP: Received non-prime: 8"

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
  ( ASemigroup a,
    FromInteger a,
    Integral a,
    KnownNat p,
    MaybeUpperBounded a,
    MEuclidean a,
    Show a,
    TestBounds a,
    ToInteger a
  ) =>
  Property
addTotal' = property $ do
  mx@(ModP.MkModP x) <- forAll (anyNat @p @a)
  my@(ModP.MkModP y) <- forAll anyNat
  let mz@(ModP.MkModP z) = mx .+. my
      z' = (toInteger x + toInteger y) `mod` p'

  annotateShow mx
  annotateShow my
  annotateShow mz

  z' === toInteger z
  where
    p' = toInteger $ natVal @p Proxy

subTotal' ::
  forall p a.
  ( AMonoid a,
    FromInteger a,
    Integral a,
    KnownNat p,
    MaybeUpperBounded a,
    MEuclidean a,
    Show a,
    TestBounds a,
    ToInteger a,
    Typeable a
  ) =>
  Property
subTotal' = property $ do
  mx@(ModP.MkModP x) <- forAll (anyNat @p @a)
  my@(ModP.MkModP y) <- forAll anyNat
  let mz@(ModP.MkModP z) = mx .-. my
      z' = (toInteger x - toInteger y) `mod` p'

  annotateShow mx
  annotateShow my
  annotateShow mz

  z' === toInteger z
  where
    p' = fromIntegral $ natVal @p Proxy

multTotal' ::
  forall p a.
  ( FromInteger a,
    Integral a,
    KnownNat p,
    MaybeUpperBounded a,
    MEuclidean a,
    Show a,
    TestBounds a,
    ToInteger a
  ) =>
  Property
multTotal' = property $ do
  mx@(ModP.MkModP x) <- forAll (anyNat @p @a)
  my@(ModP.MkModP y) <- forAll anyNat
  let mz@(ModP.MkModP z) = mx .*. my
      z' = (toInteger x * toInteger y) `mod` p'

  annotateShow mx
  annotateShow my
  annotateShow mz

  z' === toInteger z
  where
    p' = fromIntegral $ natVal @p Proxy

divTotal' ::
  forall p a.
  ( FromInteger a,
    Integral a,
    KnownNat p,
    MaybeUpperBounded a,
    MEuclidean a,
    Show a,
    TestBounds a,
    ToInteger a,
    Typeable a
  ) =>
  Property
divTotal' = property $ do
  mx <- forAll (anyNat @p @a)
  anzy <- forAll (genNZ @p @a)
  let mz = mx .%. anzy
  mx === mz .*. anzy

invert' ::
  forall p a.
  ( FromInteger a,
    Integral a,
    KnownNat p,
    MaybeUpperBounded a,
    MEuclidean a,
    Show a,
    TestBounds a,
    ToInteger a
  ) =>
  Property
invert' = property $ do
  anz <- forAll (genNZ @p @a)
  let anInv = ModP.invert anz
  ModP.reallyUnsafeModP 1 === anz .*. anInv

mkModP' ::
  forall p a.
  ( FromInteger a,
    Integral a,
    KnownNat p,
    MaybeUpperBounded a,
    MEuclidean a,
    Show a,
    ToInteger a,
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
  ( FromInteger a,
    Integral a,
    KnownNat p,
    MaybeUpperBounded a,
    MEuclidean a,
    Show a,
    ToInteger a,
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
  ( FromInteger a,
    Integral a,
    KnownNat p,
    MaybeUpperBounded a,
    MEuclidean a,
    Show a,
    ToInteger a,
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
  ( AMonoid a,
    FromInteger a,
    Integral a,
    KnownNat p,
    MaybeUpperBounded a,
    MEuclidean a,
    Show a,
    ToInteger a,
    Typeable a
  ) =>
  TestTree
boundedVals = testCase "Min/max bounds" $ do
  0 @=? ModP.unModP (minBound @(ModP.ModP p a))
  nTerm - 1 @=? ModP.unModP (maxBound @(ModP.ModP p a))
  where
    nTerm = fromIntegral $ natVal @p Proxy

genNZ ::
  forall p a.
  ( FromInteger a,
    Integral a,
    KnownNat p,
    MEuclidean a,
    TestBounds a
  ) =>
  Gen (ModP.ModP p a)
genNZ = do
  x <- HG.filter (\x' -> x' `mod` p' /= 0) $ HG.integral $ HR.exponential 2 maxVal
  let y = ModP.reallyUnsafeModP @p x
  pure y
  where
    p' = fromIntegral $ natVal @p Proxy

anyNat ::
  forall p a.
  ( FromInteger a,
    Integral a,
    KnownNat p,
    MEuclidean a,
    TestBounds a
  ) =>
  Gen (ModP.ModP p a)
anyNat = ModP.reallyUnsafeModP <$> HG.integral (HR.exponentialFrom 0 0 maxVal)

nonneg :: forall a. (Integral a, TestBounds a) => Gen a
nonneg = HG.integral $ HR.exponentialFrom 1 1 maxVal

elimProps :: TestTree
elimProps =
  testPropertyCompat desc "elimProps" $
    property $ do
      amp@(ModP.MkModP an) <- forAll (anyNat @350 @Int)

      an === ModP.unModP amp
      an === amp.unModP
      an === view #unModP amp
      an === view ModP._MkModP amp
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

extraIntProps :: [TestTree]
extraWordProps :: [TestTree]

#if USE_ARITHMOI

extraIntProps =
  [ int32Props,
    int64Props,
    intProps
  ]

int32Props :: TestTree
int32Props =
  testGroup
    "Int32"
    [ mkModPInt32,
      mkModPFailLargeInt32,
      mkModPFailCompositeInt32,
      addTotalInt32,
      subTotalInt32,
      multTotalInt32,
      divTotalInt32,
      invertInt32,
      boundedVals @2147483629 @Natural
    ]

mkModPInt32 :: TestTree
mkModPInt32 =
  testPropertyCompat "mkModP x" "mkModNInt32" $
    mkModP' @2147483629 @Int32

mkModPFailLargeInt32 :: TestTree
mkModPFailLargeInt32 =
  testPropertyCompat "mkModP fails too large" "mkModPFailLargeInt32" $
    mkModPFailSize @2147483659 @Int32 "Int32" "2147483647" "2147483659"

mkModPFailCompositeInt32 :: TestTree
mkModPFailCompositeInt32 =
  testPropertyCompat "mkModP fails for non-prime" "mkModPFailCompositeInt32" $
    mkModPFailComposite @2147483627 @Int32 "2147483627"

addTotalInt32 :: TestTree
addTotalInt32 =
  testPropertyCompat "(.+.) implements modular addition" "addTotalInt32" $
    addTotal' @2147483629 @Int32

subTotalInt32 :: TestTree
subTotalInt32 =
  testPropertyCompat "(.-.) implements modular subtraction" "subTotalInt32" $
    subTotal' @2147483629 @Int32

multTotalInt32 :: TestTree
multTotalInt32 =
  testPropertyCompat "(.*.) implements modular multiplication" "multTotalInt32" $
    multTotal' @2147483629 @Int32

divTotalInt32 :: TestTree
divTotalInt32 =
  testPropertyCompat "(.%.) implements modular division" "divTotalInt32" $
    divTotal' @2147483629 @Int32

invertInt32 :: TestTree
invertInt32 =
  testPropertyCompat "1 == x * invert x" "invertInt32" $
    invert' @2147483629 @Int32

int64Props :: TestTree
int64Props =
  testGroup
    "Int64"
    [ mkModPInt64,
      mkModPFailLargeInt64,
      mkModPFailCompositeInt64,
      addTotalInt64,
      subTotalInt64,
      multTotalInt64,
      divTotalInt64,
      invertInt64,
      boundedVals @9223372036854775783 @Natural
    ]

mkModPInt64 :: TestTree
mkModPInt64 =
  testPropertyCompat "mkModP x" "mkModNInt64" $
    mkModP' @9223372036854775783 @Int64

mkModPFailLargeInt64 :: TestTree
mkModPFailLargeInt64 =
  testPropertyCompat "mkModP fails too large" "mkModPFailLargeInt64" $
    mkModPFailSize @9223372036854775837 @Int64 "Int64" "9223372036854775807" "9223372036854775837"

mkModPFailCompositeInt64 :: TestTree
mkModPFailCompositeInt64 =
  testPropertyCompat "mkModP fails for non-prime" "mkModPFailCompositeInt64" $
    mkModPFailComposite @9223372036854775781 @Int64 "9223372036854775781"

addTotalInt64 :: TestTree
addTotalInt64 =
  testPropertyCompat "(.+.) implements modular addition" "addTotalInt64" $
    addTotal' @9223372036854775783 @Int64

subTotalInt64 :: TestTree
subTotalInt64 =
  testPropertyCompat "(.-.) implements modular subtraction" "subTotalInt64" $
    subTotal' @9223372036854775783 @Int64

multTotalInt64 :: TestTree
multTotalInt64 =
  testPropertyCompat "(.*.) implements modular multiplication" "multTotalInt64" $
    multTotal' @9223372036854775783 @Int64

divTotalInt64 :: TestTree
divTotalInt64 =
  testPropertyCompat "(.%.) implements modular division" "divTotalInt64" $
    divTotal' @9223372036854775783 @Int64

invertInt64 :: TestTree
invertInt64 =
  testPropertyCompat "1 == x * invert x" "invertInt64" $
    invert' @9223372036854775783 @Int64

intProps :: TestTree
intProps =
  testGroup
    "Int"
    [ mkModPInt,
      mkModPFailLargeInt,
      mkModPFailCompositeInt,
      addTotalInt,
      subTotalInt,
      multTotalInt,
      divTotalInt,
      invertInt
    ]

mkModPInt :: TestTree
mkModPInt =
  testPropertyCompat "mkModP x" "mkModNInt" $
    mkModP' @65521 @Int

mkModPFailLargeInt :: TestTree
mkModPFailLargeInt =
  testPropertyCompat "mkModP fails too large" "mkModPFailLargeInt" $
    mkModPFailSize @9223372036854775837 @Int "Int" "9223372036854775807" "9223372036854775837"

mkModPFailCompositeInt :: TestTree
mkModPFailCompositeInt =
  testPropertyCompat "mkModP fails for non-prime" "mkModPFailCompositeInt" $
    mkModPFailComposite @65517 @Int "65517"

addTotalInt :: TestTree
addTotalInt =
  testPropertyCompat "(.+.) implements modular addition" "addTotalInt" $
    addTotal' @65521 @Int

subTotalInt :: TestTree
subTotalInt =
  testPropertyCompat "(.-.) implements modular subtraction" "subTotalInt" $
    subTotal' @65521 @Int

multTotalInt :: TestTree
multTotalInt =
  testPropertyCompat "(.*.) implements modular multiplication" "multTotalInt" $
    multTotal' @65521 @Int

divTotalInt :: TestTree
divTotalInt =
  testPropertyCompat "(.%.) implements modular division" "divTotalInt" $
    divTotal' @65521 @Int

invertInt :: TestTree
invertInt =
  testPropertyCompat "1 == x * invert x" "invertInt" $
    invert' @65521 @Int

extraWordProps =
  [ word32Props,
    word64Props,
    wordProps
  ]

word32Props :: TestTree
word32Props =
  testGroup
    "Word32"
    [ mkModPWord32,
      mkModPFailLargeWord32,
      mkModPFailCompositeWord32,
      addTotalWord32,
      subTotalWord32,
      multTotalWord32,
      divTotalWord32,
      invertWord32
    ]

mkModPWord32 :: TestTree
mkModPWord32 =
  testPropertyCompat "mkModP x" "mkModNWord32" $
    mkModP' @4294967291 @Word32

mkModPFailLargeWord32 :: TestTree
mkModPFailLargeWord32 =
  testPropertyCompat "mkModP fails too large" "mkModPFailLargeWord32" $
    mkModPFailSize @4294967311 @Word32 "Word32" "4294967295" "4294967311"

mkModPFailCompositeWord32 :: TestTree
mkModPFailCompositeWord32 =
  testPropertyCompat "mkModP fails for non-prime" "mkModPFailCompositeWord32" $
    mkModPFailComposite @4294967289 @Word32 "4294967289"

addTotalWord32 :: TestTree
addTotalWord32 =
  testPropertyCompat "(.+.) implements modular addition" "addTotalWord32" $
    addTotal' @4294967291 @Word32

subTotalWord32 :: TestTree
subTotalWord32 =
  testPropertyCompat "(.-.) implements modular subtraction" "subTotalWord32" $
    subTotal' @4294967291 @Word32

multTotalWord32 :: TestTree
multTotalWord32 =
  testPropertyCompat "(.*.) implements modular multiplication" "multTotalWord32" $
    multTotal' @4294967291 @Word32

divTotalWord32 :: TestTree
divTotalWord32 =
  testPropertyCompat "(.%.) implements modular division" "divTotalWord32" $
    divTotal' @4294967291 @Word32

invertWord32 :: TestTree
invertWord32 =
  testPropertyCompat "1 == x * invert x" "invertWord32" $
    invert' @4294967291 @Word32

word64Props :: TestTree
word64Props =
  testGroup
    "Word64"
    [ mkModPWord64,
      mkModPFailLargeWord64,
      mkModPFailCompositeWord64,
      addTotalWord64,
      subTotalWord64,
      multTotalWord64,
      divTotalWord64,
      invertWord64
    ]

mkModPWord64 :: TestTree
mkModPWord64 =
  testPropertyCompat "mkModP x" "mkModNWord64" $
    mkModP' @18446744073709551557 @Word64

mkModPFailLargeWord64 :: TestTree
mkModPFailLargeWord64 =
  testPropertyCompat "mkModP fails too large" "mkModPFailLargeWord64" $
    mkModPFailSize @18446744073709551629 @Word64 "Word64" "18446744073709551615" "18446744073709551629"

mkModPFailCompositeWord64 :: TestTree
mkModPFailCompositeWord64 =
  testPropertyCompat "mkModP fails for non-prime" "mkModPFailCompositeWord64" $
    mkModPFailComposite @18446744073709551555 @Word64 "18446744073709551555"

addTotalWord64 :: TestTree
addTotalWord64 =
  testPropertyCompat "(.+.) implements modular addition" "addTotalWord64" $
    addTotal' @18446744073709551557 @Word64

subTotalWord64 :: TestTree
subTotalWord64 =
  testPropertyCompat "(.-.) implements modular subtraction" "subTotalWord64" $
    subTotal' @18446744073709551557 @Word64

multTotalWord64 :: TestTree
multTotalWord64 =
  testPropertyCompat "(.*.) implements modular multiplication" "multTotalWord64" $
    multTotal' @18446744073709551557 @Word64

divTotalWord64 :: TestTree
divTotalWord64 =
  testPropertyCompat "(.%.) implements modular division" "divTotalWord64" $
    divTotal' @18446744073709551557 @Word64

invertWord64 :: TestTree
invertWord64 =
  testPropertyCompat "1 == x * invert x" "invertWord64" $
    invert' @18446744073709551557 @Word64

wordProps :: TestTree
wordProps =
  testGroup
    "Word"
    [ mkModPWord,
      mkModPFailLargeWord,
      mkModPFailCompositeWord,
      addTotalWord,
      subTotalWord,
      multTotalWord,
      divTotalWord,
      invertWord
    ]

mkModPWord :: TestTree
mkModPWord =
  testPropertyCompat "mkModP x" "mkModNWord" $
    mkModP' @65521 @Word

mkModPFailLargeWord :: TestTree
mkModPFailLargeWord =
  testPropertyCompat "mkModP fails too large" "mkModPFailLargeWord" $
    mkModPFailSize @18446744073709551629 @Word "Word" "18446744073709551615" "18446744073709551629"

mkModPFailCompositeWord :: TestTree
mkModPFailCompositeWord =
  testPropertyCompat "mkModP fails for non-prime" "mkModPFailCompositeWord" $
    mkModPFailComposite @65517 @Word "65517"

addTotalWord :: TestTree
addTotalWord =
  testPropertyCompat "(.+.) implements modular addition" "addTotalWord" $
    addTotal' @65521 @Word

subTotalWord :: TestTree
subTotalWord =
  testPropertyCompat "(.-.) implements modular subtraction" "subTotalWord" $
    subTotal' @65521 @Word

multTotalWord :: TestTree
multTotalWord =
  testPropertyCompat "(.*.) implements modular multiplication" "multTotalWord" $
    multTotal' @65521 @Word

divTotalWord :: TestTree
divTotalWord =
  testPropertyCompat "(.%.) implements modular division" "divTotalWord" $
    divTotal' @65521 @Word

invertWord :: TestTree
invertWord =
  testPropertyCompat "1 == x * invert x" "invertWord" $
    invert' @65521 @Word

#else

extraIntProps = []

extraWordProps = []

#endif
