{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'NonNegative' type for enforcing a nonnegative invariant.
--
-- @since 0.1
module Numeric.Data.NonNegative
  ( -- * Type
    NonNegative (MkNonNegative),

    -- * Creation
    mkNonNegativeTH,
    mkNonNegative,
    unsafeNonNegative,
    (*!),
    reallyUnsafeNonNegative,

    -- * Elimination
    unNonNegative,

    -- * Optics
    _MkNonNegative,
    rmatching,
  )
where

import Control.DeepSeq (NFData)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Bounds
  ( LowerBounded (lowerBound),
    UpperBounded (upperBound),
    UpperBoundless,
  )
import Data.Kind (Type)
import Data.Text.Display (Display (displayBuilder))
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Language.Haskell.TH (Code, Q)
import Language.Haskell.TH.Syntax (Lift (liftTyped))
import Numeric.Algebra.Additive.AMonoid (AMonoid (zero))
import Numeric.Algebra.Additive.ASemigroup (ASemigroup ((.+.)))
import Numeric.Algebra.Multiplicative.MEuclidean (MEuclidean (mdivMod))
import Numeric.Algebra.Multiplicative.MGroup (MGroup ((.%.)))
import Numeric.Algebra.Multiplicative.MMonoid (MMonoid (one))
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup ((.*.)))
import Numeric.Algebra.Normed (Normed (norm))
import Numeric.Algebra.Semifield (Semifield)
import Numeric.Algebra.Semiring (Semiring)
import Numeric.Class.Division (Division (divide))
import Numeric.Data.NonZero (rmatching)
import Numeric.Literal.Integer (FromInteger (afromInteger))
import Numeric.Literal.Rational (FromRational (afromRational))
import Optics.Core (ReversedPrism', ReversibleOptic (re), prism)

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

-- | Unidirectional pattern synonym for 'NonNegative'. This allows us to pattern
-- match on a non-negative term without exposing the unsafe internal details.
--
-- @since 0.1
pattern MkNonNegative :: a -> NonNegative a
pattern MkNonNegative x <- UnsafeNonNegative x

{-# COMPLETE MkNonNegative #-}

-- | @since 0.1
instance (Bounded a, Num a) => Bounded (NonNegative a) where
  minBound = UnsafeNonNegative 0
  maxBound = UnsafeNonNegative maxBound
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
instance (Show a) => Display (NonNegative a) where
  displayBuilder (UnsafeNonNegative x) = displayBuilder $ show x

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
instance Normed (NonNegative a) where
  norm = id
  {-# INLINEABLE norm #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance (Num a, Ord a, Show a) => FromInteger (NonNegative a) where
  afromInteger = unsafeNonNegative . fromInteger
  {-# INLINEABLE afromInteger #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance (Fractional a, Ord a, Show a) => FromRational (NonNegative a) where
  afromRational = unsafeNonNegative . fromRational
  {-# INLINEABLE afromRational #-}

-- | @since 0.1
instance (Num a) => Semiring (NonNegative a)

-- | @since 0.1
instance (Division a, Num a) => Semifield (NonNegative a)

-- | Template haskell for creating a 'NonNegative' at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkNonNegativeTH 1)
-- UnsafeNonNegative 1
--
-- @since 0.1
mkNonNegativeTH :: (Integral a, Lift a, Show a) => a -> Code Q (NonNegative a)
mkNonNegativeTH x = maybe (error err) liftTyped $ mkNonNegative x
  where
    err =
      "Numeric.Data.NonNegative.mkNonNegativeTH: Passed value < 0: " <> show x
{-# INLINEABLE mkNonNegativeTH #-}

-- | Smart constructor for 'NonNegative'. Returns 'Nothing' if the second
-- parameter is @< 0@.
--
-- ==== __Examples__
-- >>> mkNonNegative 0
-- Just (UnsafeNonNegative 0)
--
-- >>> mkNonNegative (-2)
-- Nothing
--
-- @since 0.1
mkNonNegative :: (Num a, Ord a) => a -> Maybe (NonNegative a)
mkNonNegative x
  | x >= 0 = Just (UnsafeNonNegative x)
  | otherwise = Nothing
{-# INLINEABLE mkNonNegative #-}

-- | Variant of 'mkNonNegative' that throws an error when given a value < 0.
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
  | otherwise =
      error $
        "Numeric.Data.NonNegative.unsafeNonNegative: Passed value < 0: " <> show x
{-# INLINEABLE unsafeNonNegative #-}

-- | Postfix operator for 'unsafeNonNegative'.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
--
-- >>> (7 *!)
-- UnsafeNonNegative 7
--
-- @since 0.1
(*!) :: (HasCallStack, Num a, Ord a, Show a) => a -> NonNegative a
(*!) = unsafeNonNegative
{-# INLINE (*!) #-}

infixl 7 *!

-- | This function is an alias for the unchecked constructor @UnsafeNonNegative@
-- i.e. it allows us to construct a 'NonNegative' __without__ checking the
-- invariant. This is intended only for when we absolutely know the invariant
-- holds and a branch (i.e. 'unsafeNonNegative') is undesirable for performance
-- reasons. Exercise extreme caution.
--
-- @since 0.1
reallyUnsafeNonNegative :: a -> NonNegative a
reallyUnsafeNonNegative = UnsafeNonNegative
{-# INLINEABLE reallyUnsafeNonNegative #-}

-- | @since 0.1
unNonNegative :: NonNegative a -> a
unNonNegative (UnsafeNonNegative x) = x
{-# INLINE unNonNegative #-}

-- | 'ReversedPrism'' that enables total elimination and partial construction.
--
-- ==== __Examples__
--
-- >>> import Optics.Core ((^.))
-- >>> nn = $$(mkNonNegativeTH 2)
-- >>> nn ^. _MkNonNegative
-- 2
--
-- >>> rmatching _MkNonNegative 3
-- Right (UnsafeNonNegative 3)
--
-- >>> rmatching _MkNonNegative (-2)
-- Left (-2)
--
-- @since 0.1
_MkNonNegative :: (Num a, Ord a) => ReversedPrism' (NonNegative a) a
_MkNonNegative = re (prism unNonNegative g)
  where
    g x = case mkNonNegative x of
      Nothing -> Left x
      Just x' -> Right x'
{-# INLINEABLE _MkNonNegative #-}
