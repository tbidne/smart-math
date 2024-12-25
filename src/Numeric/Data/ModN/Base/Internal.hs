{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'ModN' type for modular arithmetic.
--
-- @since 0.1
module Numeric.Data.ModN.Base.Internal
  ( -- * Type
    ModN (MkModN, UnsafeModN),

    -- * Creation
    mkModN,
    unsafeModN,
    reallyUnsafeModN,

    -- * Misc
    errMsg,
  )
where

import Control.DeepSeq (NFData)
import Data.Bounds
  ( LowerBounded (lowerBound),
    MaybeLowerBounded (maybeLowerBound),
    MaybeUpperBounded (maybeUpperBound),
    UpperBounded (upperBound),
  )
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Text.Display (Display (displayBuilder))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Records (HasField (getField))
import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat, Nat, natVal)
import Language.Haskell.TH.Syntax (Lift)
import Numeric.Algebra.Additive.AGroup (AGroup ((.-.)))
import Numeric.Algebra.Additive.AMonoid (AMonoid (zero))
import Numeric.Algebra.Additive.ASemigroup (ASemigroup ((.+.)))
import Numeric.Algebra.MetricSpace (MetricSpace (diffR))
import Numeric.Algebra.Multiplicative.MMonoid (MMonoid (one))
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup ((.*.)))
import Numeric.Algebra.Ring (Ring)
import Numeric.Algebra.Semiring (Semiring)
import Numeric.Convert.Integer (FromInteger (fromZ), ToInteger (toZ))
import Numeric.Convert.Rational (ToRational (toQ))
import Numeric.Convert.Real (ToReal (toR))
import Numeric.Data.Internal.Utils qualified as Utils
import Optics.Core (A_Getter, LabelOptic (labelOptic), to)

-- $setup
-- >>> import Data.Int (Int8)

-- NOTE: [Safe finite modular rounding]
--
-- When creating a new @ModN n a@, we need to ensure it is safe to do so.
-- That is, n needs to fit within type a, and we want to ensure any
-- mathematical operations (e.g. multiplication) do not wrap due to a being
-- finite. Thus we have two scenarios we need to check:
--
-- 1. When we are creating a brand new @ModN n a@ (i.e. the caller is asking
--    for a specific n but does not yet have their hands on one), we need to
--    check that n is within a. We can use Utils.checkModBound via unsafeModN
--    for this.
--
-- 2. When we are combining two @ModN n a@s (e.g. addition), we have already
--    verified that the first check has passed. But we need to ensure the
--    intermediate result does not under/overflow before performing the mod.
--    We can use Utils's modSafe(Add/Mult/Sub) for this.

-- | Newtype wrapper that represents \( \mathbb{Z}/n\mathbb{Z} \).
-- 'ModN' is a 'Numeric.Algebra.Ring.Ring' i.e. supports addition, subtraction,
-- and multiplication.
--
-- When constructing a @'ModN' n a@ we must verify that the type @a@ is large
-- enough to accommodate @n@, hence the possible failure.
--
-- ==== __Examples__
--
-- >>> import Data.Text.Display (display)
-- >>> display $ unsafeModN @7 10
-- "3 (mod 7)"
--
-- @since 0.1
type ModN :: Nat -> Type -> Type
newtype ModN n a = UnsafeModN a
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Lift,
      -- | @since 0.1
      Ord
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance (KnownNat n, Show a) => Show (ModN n a) where
  -- manual so we include the mod string
  showsPrec i (UnsafeModN x) =
    showParen
      (i >= 11)
      (showString "MkModN " . showsPrec 11 x . showString modStr)
    where
      modStr = " (mod " <> show n' <> ")"
      n' = natVal @n Proxy
  {-# INLINEABLE showsPrec #-}

-- | @since 0.1
instance HasField "unModN" (ModN n a) a where
  getField (UnsafeModN x) = x

-- | @since 0.1
instance
  ( k ~ A_Getter,
    x ~ a,
    y ~ a
  ) =>
  LabelOptic "unModN" k (ModN n a) (ModN n a) x y
  where
  labelOptic = to (\(UnsafeModN x) -> x)
  {-# INLINE labelOptic #-}

-- | Bidirectional pattern synonym for 'ModN'. Construction will apply
-- modular reduction to the parameter.
--
-- @since 0.1
pattern MkModN :: a -> ModN n a
pattern MkModN x <- UnsafeModN x

{-# COMPLETE MkModN #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance
  ( Integral a,
    KnownNat n,
    MaybeUpperBounded a,
    Typeable a
  ) =>
  Bounded (ModN n a)
  where
  minBound = lowerBound
  maxBound = upperBound
  {-# INLINEABLE minBound #-}
  {-# INLINEABLE maxBound #-}

-- | @since 0.1
instance
  ( Integral a,
    KnownNat n,
    MaybeUpperBounded a,
    Typeable a
  ) =>
  LowerBounded (ModN n a)
  where
  lowerBound = unsafeModN 0
  {-# INLINEABLE lowerBound #-}

-- | @since 0.1
instance
  ( Integral a,
    KnownNat n,
    MaybeUpperBounded a,
    Typeable a
  ) =>
  UpperBounded (ModN n a)
  where
  upperBound = unsafeModN $ fromIntegral (natVal @n Proxy - 1)
  {-# INLINEABLE upperBound #-}

-- | @since 0.1
instance
  ( Integral a,
    KnownNat n,
    MaybeUpperBounded a,
    Typeable a
  ) =>
  MaybeLowerBounded (ModN n a)
  where
  maybeLowerBound = Just lowerBound
  {-# INLINEABLE maybeLowerBound #-}

-- | @since 0.1
instance
  ( Integral a,
    KnownNat n,
    MaybeUpperBounded a,
    Typeable a
  ) =>
  MaybeUpperBounded (ModN n a)
  where
  maybeUpperBound = Just upperBound
  {-# INLINEABLE maybeUpperBound #-}

-- | @since 0.1
instance (KnownNat n, Show a) => Display (ModN n a) where
  displayBuilder (UnsafeModN x) =
    mconcat
      [ displayBuilder $ show x,
        displayBuilder @String " (mod ",
        displayBuilder $ show n',
        displayBuilder @String ")"
      ]
    where
      n' = natVal @n Proxy

-- | @since 0.1
instance
  ( Integral a,
    KnownNat n,
    MaybeUpperBounded a
  ) =>
  ASemigroup (ModN n a)
  where
  UnsafeModN x .+. UnsafeModN y =
    UnsafeModN $ Utils.modSafeAdd x y (fromIntegral n')
    where
      n' = natVal @n Proxy
  {-# INLINEABLE (.+.) #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance
  ( Integral a,
    KnownNat n,
    MaybeUpperBounded a,
    Typeable a
  ) =>
  AMonoid (ModN n a)
  where
  zero = unsafeModN 0
  {-# INLINEABLE zero #-}

-- | @since 0.1
instance
  ( Integral a,
    KnownNat n,
    MaybeLowerBounded a,
    MaybeUpperBounded a,
    Typeable a
  ) =>
  AGroup (ModN n a)
  where
  UnsafeModN x .-. UnsafeModN y =
    UnsafeModN $ Utils.modSafeSub x y (fromIntegral n')
    where
      n' = natVal @n Proxy
  {-# INLINEABLE (.-.) #-}

-- | @since 0.1
instance
  ( Integral a,
    KnownNat n,
    MaybeUpperBounded a
  ) =>
  MSemigroup (ModN n a)
  where
  UnsafeModN x .*. UnsafeModN y =
    UnsafeModN $ Utils.modSafeMult x y (fromIntegral n')
    where
      n' = natVal @n Proxy
  {-# INLINEABLE (.*.) #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance
  ( Integral a,
    KnownNat n,
    MaybeUpperBounded a,
    Typeable a
  ) =>
  MMonoid (ModN n a)
  where
  one = unsafeModN 1
  {-# INLINEABLE one #-}

-- | @since 0.1
instance
  ( Integral a,
    KnownNat n,
    MaybeUpperBounded a,
    Typeable a
  ) =>
  Semiring (ModN n a)

-- | @since 0.1
instance
  ( Integral a,
    KnownNat n,
    MaybeLowerBounded a,
    MaybeUpperBounded a,
    Typeable a
  ) =>
  Ring (ModN n a)

-- | @since 0.1
instance
  ( Integral a,
    KnownNat n,
    MaybeLowerBounded a,
    MaybeUpperBounded a,
    Typeable a
  ) =>
  MetricSpace (ModN n a)
  where
  diffR x y = realToFrac d
    where
      UnsafeModN d = y .-. x
  {-# INLINEABLE diffR #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance
  ( Integral a,
    KnownNat n,
    MaybeUpperBounded a,
    Typeable a
  ) =>
  FromInteger (ModN n a)
  where
  fromZ = unsafeModN . fromInteger
  {-# INLINEABLE fromZ #-}

-- | @since 0.1
instance (Integral a) => ToInteger (ModN n a) where
  toZ (UnsafeModN x) = toInteger x
  {-# INLINEABLE toZ #-}

-- | @since 0.1
instance (Real a) => ToRational (ModN n a) where
  toQ (UnsafeModN x) = toRational x
  {-# INLINEABLE toQ #-}

-- | @since 0.1
instance (Real a) => ToReal (ModN n a) where
  toR (UnsafeModN x) = realToFrac x
  {-# INLINEABLE toR #-}

-- | Constructor for 'ModN'.
--
-- ==== __Examples__
-- >>> mkModN @5 7
-- Right (MkModN 2 (mod 5))
--
-- >>> mkModN @10 7
-- Right (MkModN 7 (mod 10))
--
-- >>> mkModN @128 (9 :: Int8)
-- Left "Type 'Int8' has a maximum size of 127. This is not large enough to safely implement mod 128."
--
-- @since 0.1
mkModN ::
  forall n a.
  ( Integral a,
    KnownNat n,
    MaybeUpperBounded a,
    Typeable a
  ) =>
  a ->
  Either String (ModN n a)
mkModN x = maybe modN Left (Utils.checkModBound x n')
  where
    modN = Right x'
    n' = toInteger $ natVal @n Proxy
    x' = UnsafeModN $ x `mod` fromIntegral n'
{-# INLINEABLE mkModN #-}

-- | Variant of 'mkModN' that throws an error when type @a@ is not
-- large enough to fit @n@.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> unsafeModN @7 12
-- MkModN 5 (mod 7)
--
-- @since 0.1
unsafeModN ::
  forall n a.
  ( HasCallStack,
    Integral a,
    KnownNat n,
    MaybeUpperBounded a,
    Typeable a
  ) =>
  a ->
  ModN n a
unsafeModN x = case mkModN x of
  Right mp -> mp
  Left err -> error $ errMsg "unsafeModN" err
{-# INLINEABLE unsafeModN #-}

-- | This function reduces the argument modulo @p@ but does __not__ check
-- that @n@ fits within a. Note that correct behavior requires this, so this
-- is dangerous. This is intended only for when we absolutely know @n@ fits in
-- @a@ and the check is undesirable for performance reasons. Exercise extreme
-- caution.
--
-- @since 0.1
reallyUnsafeModN :: forall n a. (Integral a, KnownNat n) => a -> ModN n a
reallyUnsafeModN = UnsafeModN . (`mod` n')
  where
    n' = fromIntegral $ natVal @n Proxy
{-# INLINEABLE reallyUnsafeModN #-}

-- | @since 0.1
errMsg :: String -> String -> String
errMsg fn msg =
  mconcat
    [ "Numeric.Data.ModN.Base.",
      fn,
      ": ",
      msg
    ]
