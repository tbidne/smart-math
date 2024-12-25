{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'NonNegative' type for enforcing a nonnegative invariant.
--
-- @since 0.1
module Numeric.Data.NonNegative.Base.Internal
  ( -- * Type
    NonNegative (MkNonNegative, UnsafeNonNegative),

    -- * Creation
    unsafeNonNegative,

    -- * Misc
    errMsg,
  )
where

import Control.DeepSeq (NFData)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Bounds
  ( LowerBounded (lowerBound),
    MaybeLowerBounded (maybeLowerBound),
    MaybeUpperBounded (maybeUpperBound),
    UpperBounded (upperBound),
    UpperBoundless,
  )
import Data.Kind (Type)
import Data.Text.Display (Display, ShowInstance (ShowInstance))
import GHC.Generics (Generic)
import GHC.Records (HasField (getField))
import GHC.Stack (HasCallStack)
import Language.Haskell.TH.Syntax (Lift)
import Numeric.Algebra.Additive.AMonoid (AMonoid (zero))
import Numeric.Algebra.Additive.ASemigroup (ASemigroup ((.+.)))
import Numeric.Algebra.MetricSpace (MetricSpace (diffR))
import Numeric.Algebra.Multiplicative.MEuclidean (MEuclidean (mdivMod))
import Numeric.Algebra.Multiplicative.MGroup (MGroup ((.%.)))
import Numeric.Algebra.Multiplicative.MMonoid (MMonoid (one))
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup ((.*.)))
import Numeric.Algebra.Normed (Normed (norm))
import Numeric.Algebra.Semifield (Semifield)
import Numeric.Algebra.Semiring (Semiring)
import Numeric.Class.Division (Division (divide))
import Numeric.Convert.Integer (FromInteger (fromZ), ToInteger (toZ))
import Numeric.Convert.Rational (FromRational (fromQ), ToRational (toQ))
import Numeric.Convert.Real (FromReal (fromR), ToReal (toR))
import Numeric.Data.Internal.Utils qualified as Utils
import Optics.Core (A_Getter, LabelOptic (labelOptic), to)

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> :set -XPostfixOperators

-- | Newtype wrapper that attaches a 'NonNegative' invariant to some @a@.
-- 'NonNegative' is a 'Numeric.Algebra.Semifield.Semifield' i.e. supports
-- addition, multiplication, and division.
--
-- @since 0.1
type NonNegative :: Type -> Type
newtype NonNegative a = UnsafeNonNegative a
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Lift,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData,
      -- | @since 0.1
      UpperBoundless
    )
  deriving
    ( -- | @since 0.1
      Display
    )
    via (ShowInstance a)

-- | @since 0.1
instance HasField "unNonNegative" (NonNegative a) a where
  getField (UnsafeNonNegative x) = x

-- | @since 0.1
instance
  ( k ~ A_Getter,
    x ~ a,
    y ~ a
  ) =>
  LabelOptic "unNonNegative" k (NonNegative a) (NonNegative a) x y
  where
  labelOptic = to (\(UnsafeNonNegative x) -> x)
  {-# INLINE labelOptic #-}

-- | Unidirectional pattern synonym for 'NonNegative'. This allows us to pattern
-- match on a non-negative term without exposing the unsafe internal details.
--
-- @since 0.1
pattern MkNonNegative :: a -> NonNegative a
pattern MkNonNegative x <- UnsafeNonNegative x

{-# COMPLETE MkNonNegative #-}

-- | @since 0.1
instance (Num a, UpperBounded a) => Bounded (NonNegative a) where
  minBound = lowerBound
  maxBound = upperBound
  {-# INLINEABLE minBound #-}
  {-# INLINEABLE maxBound #-}

-- | @since 0.1
instance (Num a) => LowerBounded (NonNegative a) where
  lowerBound = UnsafeNonNegative 0
  {-# INLINEABLE lowerBound #-}

-- | @since 0.1
instance (UpperBounded a) => UpperBounded (NonNegative a) where
  upperBound = UnsafeNonNegative upperBound
  {-# INLINEABLE upperBound #-}

-- | @since 0.1
instance (Num a) => MaybeLowerBounded (NonNegative a) where
  maybeLowerBound = Just lowerBound
  {-# INLINEABLE maybeLowerBound #-}

-- | @since 0.1
instance (MaybeUpperBounded a) => MaybeUpperBounded (NonNegative a) where
  maybeUpperBound = UnsafeNonNegative <$> maybeUpperBound
  {-# INLINEABLE maybeUpperBound #-}

-- | @since 0.1
instance (Num a) => ASemigroup (NonNegative a) where
  UnsafeNonNegative x .+. UnsafeNonNegative y = UnsafeNonNegative $ x + y
  {-# INLINEABLE (.+.) #-}

-- | @since 0.1
instance (Num a) => AMonoid (NonNegative a) where
  zero = UnsafeNonNegative 0
  {-# INLINEABLE zero #-}

-- | @since 0.1
instance (Num a) => MSemigroup (NonNegative a) where
  UnsafeNonNegative x .*. UnsafeNonNegative y = UnsafeNonNegative $ x * y
  {-# INLINEABLE (.*.) #-}

-- | @since 0.1
instance (Num a) => MMonoid (NonNegative a) where
  one = UnsafeNonNegative 1
  {-# INLINEABLE one #-}

-- | @since 0.1
instance (Division a, Num a) => MGroup (NonNegative a) where
  UnsafeNonNegative x .%. (UnsafeNonNegative d) = UnsafeNonNegative $ x `divide` d
  {-# INLINEABLE (.%.) #-}

-- | @since 0.1
instance (Division a, Integral a) => MEuclidean (NonNegative a) where
  UnsafeNonNegative x `mdivMod` (UnsafeNonNegative d) =
    bimap UnsafeNonNegative UnsafeNonNegative $ x `divMod` d
  {-# INLINEABLE mdivMod #-}

-- | @since 0.1
instance (Real a) => MetricSpace (NonNegative a) where
  diffR (UnsafeNonNegative x) (UnsafeNonNegative y) = Utils.safeDiff x y
  {-# INLINEABLE diffR #-}

-- | @since 0.1
instance Normed (NonNegative a) where
  norm = id
  {-# INLINEABLE norm #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance (Num a, Ord a, Show a) => FromInteger (NonNegative a) where
  fromZ = unsafeNonNegative . fromInteger
  {-# INLINEABLE fromZ #-}

-- | @since 0.1
instance (Integral a) => ToInteger (NonNegative a) where
  toZ (UnsafeNonNegative x) = toInteger x
  {-# INLINEABLE toZ #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance (Fractional a, Ord a, Show a) => FromRational (NonNegative a) where
  fromQ = unsafeNonNegative . fromRational
  {-# INLINEABLE fromQ #-}

-- | @since 0.1
instance (Real a) => ToRational (NonNegative a) where
  toQ (UnsafeNonNegative x) = toRational x
  {-# INLINEABLE toQ #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance (Fractional a, Ord a, Show a) => FromReal (NonNegative a) where
  fromR = unsafeNonNegative . realToFrac
  {-# INLINEABLE fromR #-}

-- | @since 0.1
instance (Real a) => ToReal (NonNegative a) where
  toR (UnsafeNonNegative x) = realToFrac x
  {-# INLINEABLE toR #-}

-- | @since 0.1
instance (Num a) => Semiring (NonNegative a)

-- | @since 0.1
instance (Division a, Num a) => Semifield (NonNegative a)

-- | Throws an error when given a value < 0.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> unsafeNonNegative 7
-- UnsafeNonNegative 7
--
-- @since 0.1
unsafeNonNegative :: (HasCallStack, Num a, Ord a, Show a) => a -> NonNegative a
unsafeNonNegative x
  | x >= 0 = UnsafeNonNegative x
  | otherwise = error $ errMsg "unsafeNonNegative" x
{-# INLINEABLE unsafeNonNegative #-}

-- | @since 0.1
errMsg :: (Show a) => String -> a -> String
errMsg fn x =
  mconcat
    [ "Numeric.Data.NonNegative.Base.",
      fn,
      ": Received value < zero: ",
      show x
    ]
