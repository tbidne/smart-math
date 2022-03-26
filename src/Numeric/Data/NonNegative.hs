{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Provides the 'NonNegative' type for enforcing a nonnegative invariant.
--
-- @since 0.1.0.0
module Numeric.Data.NonNegative
  ( -- * Type
    NonNegative (MkNonNegative),

    -- * Creation
    mkNonNegativeTH,
    mkNonNegative,
    unsafeNonNegative,
    reallyUnsafeNonNegative,

    -- * Elimination
    unNonNegative,
  )
where

import Control.DeepSeq (NFData)
import Data.Bifunctor (Bifunctor (..))
import Data.Kind (Type)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
#if MIN_VERSION_template_haskell(2, 17, 0)
import Language.Haskell.TH (Code, Q)
#else
import Language.Haskell.TH (Q, TExp)
#endif
import Language.Haskell.TH.Syntax (Lift (..))
import Numeric.Algebra.Additive.AMonoid (AMonoid (..))
import Numeric.Algebra.Additive.ASemigroup (ASemigroup (..))
import Numeric.Algebra.Multiplicative.MGroup (MGroup (..), MGroupIntegral (..))
import Numeric.Algebra.Multiplicative.MMonoid (MMonoid (..))
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup (..))
import Numeric.Algebra.Semiring (Semiring)
import Numeric.Class.Division (Division (..))
import Numeric.Data.NonZero (NonZero (..))

-- $setup
-- >>> :set -XTemplateHaskell

-- | Newtype wrapper that attaches a 'NonNegative' invariant to some @a@.
-- 'NonNegative' is a:
--
-- * 'Numeric.Algebra.Additive.ASemigroup.ASemigroup'
-- * 'Numeric.Algebra.Additive.AMonoid.AMonoid'
-- * 'Numeric.Algebra.Multiplicative.MSemigroup.MSemigroup'
-- * 'Numeric.Algebra.Multiplicative.MMonoid.MMonoid'
-- * 'Numeric.Algebra.Multiplicative.MGroup.MGroup'
-- * 'Numeric.Algebra.Multiplicative.MGroup.MGroupIntegral'
-- * 'Numeric.Algebra.Semiring.Semiring'
--
-- @since 0.1.0.0
type NonNegative :: Type -> Type
newtype NonNegative a = UnsafeNonNegative a
  deriving stock
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Generic,
      -- | @since 0.1.0.0
      Lift,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Show
    )
  deriving anyclass
    ( -- | @since 0.1.0.0
      NFData
    )

-- | Bidirectional pattern synonym for 'NonNegative'. Construction fails when
-- the given value is positive.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> MkNonNegative 0
-- UnsafeNonNegative 0
--
-- @since 0.1.0.0
pattern MkNonNegative :: (Num a, Ord a, Show a) => a -> NonNegative a
pattern MkNonNegative x <-
  UnsafeNonNegative x
  where
    MkNonNegative x = unsafeNonNegative x

{-# COMPLETE MkNonNegative #-}

-- | @since 0.1.0.0
instance (Eq a, Num a, Ord a, Show a) => ASemigroup (NonNegative a) where
  type AddConstraint (NonNegative a) = NonNegative a
  MkNonNegative x .+. MkNonNegative y = reallyUnsafeNonNegative $ x + y

-- | @since 0.1.0.0
instance (Eq a, Num a, Ord a, Show a) => AMonoid (NonNegative a) where
  zero = reallyUnsafeNonNegative 0

-- | @since 0.1.0.0
instance (Eq a, Num a, Ord a, Show a) => MSemigroup (NonNegative a) where
  type MultConstraint (NonNegative a) = NonNegative a
  MkNonNegative x .*. MkNonNegative y = reallyUnsafeNonNegative $ x * y

-- | @since 0.1.0.0
instance (Eq a, Num a, Ord a, Show a) => MMonoid (NonNegative a) where
  one = reallyUnsafeNonNegative 1

-- | @since 0.1.0.0
instance (Eq a, Division a, Num a, Ord a, Show a) => MGroup (NonNegative a) where
  type DivConstraint (NonNegative a) = NonZero (NonNegative a)
  MkNonNegative x .%. MkNonZero (MkNonNegative d) = reallyUnsafeNonNegative $ x `divide` d

-- | @since 0.1.0.0
instance (Division a, Integral a, Show a) => MGroupIntegral (NonNegative a) where
  type ModResult (NonNegative a) = NonNegative a
  MkNonNegative x `gdivMod` MkNonZero (MkNonNegative d) =
    bimap UnsafeNonNegative UnsafeNonNegative $ x `divMod` d

-- | @since 0.1.0.0
instance (Eq a, Num a, Ord a, Show a) => Semiring (NonNegative a)

-- | Unwraps a 'NonNegative'.
--
-- @since 0.1.0.0
unNonNegative :: NonNegative a -> a
unNonNegative (UnsafeNonNegative x) = x

-- | Template haskell for creating a 'NonNegative' at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkNonNegativeTH 1)
-- UnsafeNonNegative 1
--
-- @since 0.1.0.0
#if MIN_VERSION_template_haskell(2,17,0)
mkNonNegativeTH :: (Integral a, Lift a, Show a) => a -> Code Q (NonNegative a)
#else
mkNonNegativeTH :: (Integral a, Lift a, Show a) => a -> Q (TExp (NonNegative a))
#endif
mkNonNegativeTH x = maybe (error err) liftTyped $ mkNonNegative x
  where
    err =
      "Numeric.Data.NonNegative.mkNonNegativeTH: Passed value < 0: " <> show x

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
-- @since 0.1.0.0
mkNonNegative :: (Num a, Ord a) => a -> Maybe (NonNegative a)
mkNonNegative x
  | x >= 0 = Just (UnsafeNonNegative x)
  | otherwise = Nothing

-- | Variant of 'mkNonNegative' that throws an error when given a value < 0.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> unsafeNonNegative 7
-- UnsafeNonNegative 7
--
-- @since 0.1.0.0
unsafeNonNegative :: (HasCallStack, Num a, Ord a, Show a) => a -> NonNegative a
unsafeNonNegative x
  | x >= 0 = UnsafeNonNegative x
  | otherwise =
      error $
        "Numeric.Data.NonNegative.unsafeNonNegative: Passed value < 0: " <> show x

-- | This function is an alias for the unchecked constructor @UnsafeNonNegative@
-- i.e. it allows us to construct a 'NonNegative' __without__ checking the
-- invariant. This is intended only for when we absolutely know the invariant
-- holds and a branch (i.e. 'unsafeNonNegative') is undesirable for performance
-- reasons. Exercise extreme caution.
--
-- @since 0.1.0.0
reallyUnsafeNonNegative :: a -> NonNegative a
reallyUnsafeNonNegative = UnsafeNonNegative
