{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-identities #-}

-- See the note on Modulus for why this warning is disabled

-- | Internal module for 'ModP'.
--
-- @since 0.1
module Numeric.Data.ModP.Internal
  ( -- * Type
    ModP (MkModP, ..),

    -- ** Functions
    mkModP,
    unsafeModP,
    invert,
    reallyUnsafeModP,

    -- ** Misc
    Prime.errMsg,
  )
where

import Control.DeepSeq (NFData)
import Data.Bounds
  ( LowerBounded (lowerBound),
    MaybeLowerBounded (maybeLowerBound),
    MaybeUpperBounded (maybeUpperBound),
    UpperBounded (upperBound),
  )
import Data.Data (Proxy (Proxy))
import Data.Kind (Type)
import Data.Text.Display (Display (displayBuilder))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Records (HasField (getField))
import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat, Nat, natVal)
import Language.Haskell.TH.Syntax (Lift)
import Numeric.Algebra (MEuclidean)
import Numeric.Algebra.Additive.AGroup (AGroup ((.-.)))
import Numeric.Algebra.Additive.AMonoid (AMonoid (zero))
import Numeric.Algebra.Additive.ASemigroup (ASemigroup ((.+.)))
import Numeric.Algebra.Field (Field)
import Numeric.Algebra.MetricSpace (MetricSpace (diffR))
import Numeric.Algebra.Multiplicative.MEuclidean (mmod)
import Numeric.Algebra.Multiplicative.MGroup (MGroup ((.%.)))
import Numeric.Algebra.Multiplicative.MMonoid (MMonoid (one))
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup ((.*.)))
import Numeric.Algebra.Ring (Ring)
import Numeric.Algebra.Semifield (Semifield)
import Numeric.Algebra.Semiring (Semiring)
import Numeric.Convert.Integer (FromInteger (fromZ), ToInteger (toZ))
import Numeric.Convert.Rational (ToRational (toQ))
import Numeric.Convert.Real (ToReal (toR))
import Numeric.Data.Internal.Utils qualified as Utils
import Numeric.Data.ModP.Internal.Primality
  ( MaybePrime
      ( Composite,
        ProbablyPrime
      ),
  )
import Numeric.Data.ModP.Internal.Primality qualified as Prime
import Optics.Core (A_Getter, LabelOptic (labelOptic), to)

-- $setup
-- >>> import Data.Int (Int8)

-- see NOTE: [Safe finite modular rounding]

-- | Newtype wrapper that represents \( \mathbb{Z}/p\mathbb{Z} \) for prime @p@.
-- 'ModP' is a 'Numeric.Algebra.Field.Field' i.e. supports addition,
-- subtraction, multiplication, and division.
--
-- When constructing a @'ModP' p a@ we must verify that @p@ is prime and the
-- type @a@ is large enough to accommodate @p@, hence the possible failure.
--
-- ==== __Examples__
--
-- >>> import Data.Text.Display (display)
-- >>> display $ unsafeModP @7 10
-- "3 (mod 7)"
--
-- @since 0.1
type ModP :: Nat -> Type -> Type
newtype ModP p a = UnsafeModP a
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Foldable,
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

-- | Unidirectional pattern synonym for 'ModP'. This allows us to pattern
-- match on a modp term without exposing the unsafe internal details.
--
-- @since 0.1
pattern MkModP :: a -> ModP p a
pattern MkModP x <- UnsafeModP x

{-# COMPLETE MkModP #-}

-- | @since 0.1
instance (KnownNat p, Show a) => Show (ModP p a) where
  -- manual so we show "MkModP" instead of "UnsafeModP"
  showsPrec i (UnsafeModP x) =
    showParen
      (i >= 11)
      (showString "MkModP " . showsPrec 11 x . showString modStr)
    where
      modStr = " (mod " <> show p' <> ")"
      p' = natVal @p Proxy
  {-# INLINEABLE showsPrec #-}

-- | @since 0.1
instance HasField "unModP" (ModP p a) a where
  getField (UnsafeModP x) = x

-- | @since 0.1
instance
  ( k ~ A_Getter,
    x ~ a,
    y ~ a
  ) =>
  LabelOptic "unModP" k (ModP p a) (ModP p a) x y
  where
  labelOptic = to (\(UnsafeModP x) -> x)
  {-# INLINE labelOptic #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance
  ( AMonoid a,
    FromInteger a,
    KnownNat p,
    MaybeUpperBounded a,
    MEuclidean a,
    ToInteger a,
    Typeable a
  ) =>
  Bounded (ModP p a)
  where
  minBound = lowerBound
  maxBound = upperBound
  {-# INLINEABLE minBound #-}
  {-# INLINEABLE maxBound #-}

-- | @since 0.1
instance
  ( AMonoid a,
    FromInteger a,
    KnownNat p,
    MaybeUpperBounded a,
    MEuclidean a,
    ToInteger a,
    Typeable a
  ) =>
  LowerBounded (ModP p a)
  where
  lowerBound = unsafeModP zero
  {-# INLINEABLE lowerBound #-}

-- | @since 0.1
instance
  ( FromInteger a,
    KnownNat p,
    MaybeUpperBounded a,
    MEuclidean a,
    ToInteger a,
    Typeable a
  ) =>
  UpperBounded (ModP p a)
  where
  upperBound = unsafeModP $ fromZ $ toZ (natVal @p Proxy - 1)
  {-# INLINEABLE upperBound #-}

-- | @since 0.1
instance
  ( AMonoid a,
    FromInteger a,
    KnownNat p,
    MaybeUpperBounded a,
    MEuclidean a,
    ToInteger a,
    Typeable a
  ) =>
  MaybeLowerBounded (ModP p a)
  where
  maybeLowerBound = Just lowerBound
  {-# INLINEABLE maybeLowerBound #-}

-- | @since 0.1
instance
  ( FromInteger a,
    KnownNat p,
    MaybeUpperBounded a,
    MEuclidean a,
    ToInteger a,
    Typeable a
  ) =>
  MaybeUpperBounded (ModP p a)
  where
  maybeUpperBound = Just upperBound
  {-# INLINEABLE maybeUpperBound #-}

-- | @since 0.1
instance (KnownNat p, Show a) => Display (ModP p a) where
  displayBuilder (UnsafeModP x) =
    mconcat
      [ displayBuilder $ show x,
        displayBuilder @String " (mod ",
        displayBuilder $ show p',
        displayBuilder @String ")"
      ]
    where
      p' = natVal @p Proxy

-- | @since 0.1
instance
  ( ASemigroup a,
    FromInteger a,
    KnownNat p,
    MaybeUpperBounded a,
    MEuclidean a,
    ToInteger a
  ) =>
  ASemigroup (ModP p a)
  where
  UnsafeModP x .+. UnsafeModP y =
    UnsafeModP $ Utils.modSafeAddAlgebra x y (fromZ $ toZ p')
    where
      p' = natVal @p Proxy
  {-# INLINEABLE (.+.) #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance
  ( AMonoid a,
    FromInteger a,
    KnownNat p,
    MaybeUpperBounded a,
    MEuclidean a,
    ToInteger a,
    Typeable a
  ) =>
  AMonoid (ModP p a)
  where
  zero = unsafeModP zero
  {-# INLINEABLE zero #-}

-- | @since 0.1
instance
  ( AMonoid a,
    FromInteger a,
    KnownNat p,
    MaybeUpperBounded a,
    MEuclidean a,
    ToInteger a,
    Typeable a
  ) =>
  AGroup (ModP p a)
  where
  UnsafeModP x .-. UnsafeModP y =
    UnsafeModP $ Utils.modSafeSubAlgebra x y (fromZ $ toZ p')
    where
      p' = natVal @p Proxy
  {-# INLINEABLE (.-.) #-}

-- | @since 0.1
instance
  ( FromInteger a,
    KnownNat p,
    MaybeUpperBounded a,
    MEuclidean a,
    ToInteger a
  ) =>
  MSemigroup (ModP p a)
  where
  UnsafeModP x .*. UnsafeModP y =
    UnsafeModP $ Utils.modSafeMultAlgebra x y (fromZ $ toZ p')
    where
      p' = natVal @p Proxy
  {-# INLINEABLE (.*.) #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance
  ( FromInteger a,
    KnownNat p,
    MaybeUpperBounded a,
    MEuclidean a,
    ToInteger a,
    Typeable a
  ) =>
  MMonoid (ModP p a)
  where
  one = unsafeModP one
  {-# INLINEABLE one #-}

-- | @since 0.1
instance
  ( FromInteger a,
    KnownNat p,
    MaybeUpperBounded a,
    MEuclidean a,
    ToInteger a,
    Typeable a
  ) =>
  MGroup (ModP p a)
  where
  x .%. d = x .*. invert d
  {-# INLINEABLE (.%.) #-}

-- | @since 0.1
instance
  ( AMonoid a,
    FromInteger a,
    KnownNat p,
    MaybeUpperBounded a,
    MEuclidean a,
    ToInteger a,
    Typeable a
  ) =>
  Semiring (ModP p a)

-- | @since 0.1
instance
  ( AMonoid a,
    FromInteger a,
    KnownNat p,
    MaybeUpperBounded a,
    MEuclidean a,
    ToInteger a,
    Typeable a
  ) =>
  Ring (ModP p a)

-- | @since 0.1
instance
  ( AMonoid a,
    FromInteger a,
    KnownNat p,
    MaybeUpperBounded a,
    MEuclidean a,
    ToInteger a,
    Typeable a
  ) =>
  Semifield (ModP p a)

-- | @since 0.1
instance
  ( AMonoid a,
    FromInteger a,
    KnownNat p,
    MaybeUpperBounded a,
    MEuclidean a,
    ToInteger a,
    Typeable a
  ) =>
  Field (ModP p a)

-- | @since 0.1
instance
  ( AMonoid a,
    FromInteger a,
    KnownNat p,
    MaybeUpperBounded a,
    MEuclidean a,
    ToInteger a,
    Typeable a
  ) =>
  MetricSpace (ModP p a)
  where
  diffR x y = toR d
    where
      UnsafeModP d = y .-. x
  {-# INLINEABLE diffR #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance
  ( FromInteger a,
    KnownNat p,
    MaybeUpperBounded a,
    MEuclidean a,
    ToInteger a,
    Typeable a
  ) =>
  FromInteger (ModP p a)
  where
  fromZ = unsafeModP . fromZ
  {-# INLINEABLE fromZ #-}

-- | @since 0.1
instance (ToInteger a) => ToInteger (ModP p a) where
  toZ (UnsafeModP x) = toZ x
  {-# INLINEABLE toZ #-}

-- | @since 0.1
instance (ToRational a) => ToRational (ModP p a) where
  toQ (UnsafeModP x) = toQ x
  {-# INLINEABLE toQ #-}

-- | @since 0.1
instance (ToReal a) => ToReal (ModP p a) where
  toR (UnsafeModP x) = toR x
  {-# INLINEABLE toR #-}

-- | Constructor for 'ModP'. Fails if @p@ is not prime. This uses the
-- Miller-Rabin primality test, which has complexity \(O(k \log^3 p)\), and we
-- take \(k = 100\). See
-- [wikipedia](https://en.wikipedia.org/wiki/Miller-Rabin_primality_test#Complexity)
-- for more details.
--
-- ==== __Examples__
-- >>> mkModP @5 7
-- Right (MkModP 2 (mod 5))
--
-- >>> mkModP @10 7
-- Left "Received non-prime: 10"
--
-- >>> mkModP @128 (9 :: Int8)
-- Left "Type 'Int8' has a maximum size of 127. This is not large enough to safely implement mod 128."
--
-- @since 0.1
mkModP ::
  forall p a.
  ( FromInteger a,
    KnownNat p,
    MaybeUpperBounded a,
    MEuclidean a,
    ToInteger a,
    Typeable a
  ) =>
  a ->
  Either String (ModP p a)
mkModP x = maybe modP Left (Utils.checkModBoundAlgebra x p')
  where
    modP = case Prime.isPrime p' of
      Composite -> Left $ "Received non-prime: " <> show p'
      ProbablyPrime -> Right $ UnsafeModP x'

    p' = toZ $ natVal @p Proxy
    x' = x `mmod` fromZ p'
{-# INLINEABLE mkModP #-}

-- | Variant of 'mkModP' that throws an error when given a non-prime.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> unsafeModP @7 12
-- MkModP 5 (mod 7)
--
-- @since 0.1
unsafeModP ::
  forall p a.
  ( FromInteger a,
    HasCallStack,
    KnownNat p,
    MaybeUpperBounded a,
    MEuclidean a,
    ToInteger a,
    Typeable a
  ) =>
  a ->
  ModP p a
unsafeModP x = case mkModP x of
  Right mp -> mp
  Left err -> error $ Prime.errMsg "unsafeModP" err
{-# INLINEABLE unsafeModP #-}

-- | Given non-zero \(d\), returns the inverse i.e. finds \(e\) s.t.
--
-- \[
-- de \equiv 1 \pmod p.
-- \]
--
-- ==== __Examples__
-- findInverse
-- >>> invert $ unsafeModP @7 5
-- MkModP 3 (mod 7)
--
-- >>> invert $ unsafeModP @19 12
-- MkModP 8 (mod 19)
--
-- @since 0.1
invert ::
  forall p a.
  ( FromInteger a,
    KnownNat p,
    MEuclidean a,
    ToInteger a
  ) =>
  ModP p a ->
  ModP p a
invert (UnsafeModP d) =
  reallyUnsafeModP $ fromZ $ toZ $ Prime.invert @p (fromZ $ toZ d)
{-# INLINEABLE invert #-}

-- | This function reduces the argument modulo @p@ but does __not__ check
-- that @p@ is prime. Note that the correct behavior of some functionality
-- (e.g. division) is reliant on primality, so this is dangerous. This is
-- intended only for when we absolutely know @p@ is prime and the check
-- is undesirable for performance reasons. Exercise extreme caution.
--
-- @since 0.1
reallyUnsafeModP ::
  forall p a.
  ( FromInteger a,
    KnownNat p,
    MEuclidean a
  ) =>
  a ->
  ModP p a
reallyUnsafeModP = UnsafeModP . (`mmod` p')
  where
    p' = fromZ $ toZ $ natVal @p Proxy
{-# INLINEABLE reallyUnsafeModP #-}
