{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Provides the 'Positive' type for enforcing a positive invariant.
--
-- @since 0.1
module Numeric.Data.Positive
  ( -- * Type
    Positive (MkPositive),

    -- * Creation
    mkPositiveTH,
    mkPositive,
    unsafePositive,
    reallyUnsafePositive,

    -- * Elimination
    unPositive,

    -- * Functions
    positiveToNonZero,
  )
where

import Control.DeepSeq (NFData)
import Data.Kind (Type)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
#if MIN_VERSION_template_haskell(2, 17, 0)
import Language.Haskell.TH (Code, Q)
#else
import Language.Haskell.TH (Q, TExp)
#endif
import Language.Haskell.TH.Syntax (Lift (..))
import Numeric.Algebra.Additive.ASemigroup (ASemigroup (..))
import Numeric.Algebra.Multiplicative.MGroup (MGroup (..))
import Numeric.Algebra.Multiplicative.MMonoid (MMonoid (..))
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup (..))
import Numeric.Class.Division (Division (..))
import Numeric.Class.Literal (NumLiteral (..))
import Numeric.Data.NonZero (NonZero (..), reallyUnsafeNonZero)

-- $setup
-- >>> :set -XTemplateHaskell

-- | Newtype wrapper that attaches a 'Positive' invariant to some @a@.
-- 'Positive' is a:
--
-- * 'Numeric.Algebra.Additive.ASemigroup.ASemigroup'
-- * 'Numeric.Algebra.Multiplicative.MSemigroup.MSemigroup'
-- * 'Numeric.Algebra.Multiplicative.MMonoid.MMonoid'
-- * 'Numeric.Algebra.Multiplicative.MGroup.MGroup'
-- * 'Numeric.Algebra.Multiplicative.MGroup.MGroupIntegral'
--
-- @since 0.1
type Positive :: Type -> Type
newtype Positive a = UnsafePositive a
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
      NFData
    )

-- | Bidirectional pattern synonym for 'Positive'. Construction fails when
-- the given value is non-positive.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> MkPositive 7
-- UnsafePositive 7
--
-- @since 0.1
pattern MkPositive :: (Num a, Ord a, Show a) => a -> Positive a
pattern MkPositive x <-
  UnsafePositive x
  where
    MkPositive x = unsafePositive x

{-# COMPLETE MkPositive #-}

-- | @since 0.1
instance (Eq a, Num a) => ASemigroup (Positive a) where
  UnsafePositive x .+. UnsafePositive y = UnsafePositive $ x + y

-- | @since 0.1
instance (Eq a, Num a) => MSemigroup (Positive a) where
  UnsafePositive x .*. UnsafePositive y = UnsafePositive $ x * y

-- | @since 0.1
instance (Eq a, Num a) => MMonoid (Positive a) where
  one = UnsafePositive 1

-- | @since 0.1
instance (Eq a, Division a, Num a) => MGroup (Positive a) where
  UnsafePositive x .%. MkNonZero (UnsafePositive d) = UnsafePositive $ x `divide` d

-- | __WARNING: Partial__
--
-- @since 0.1
instance (Num a, Ord a, Show a) => NumLiteral (Positive a) where
  fromLit = unsafePositive . fromInteger

-- | Unwraps a 'Positive'.
--
-- @since 0.1
unPositive :: Positive a -> a
unPositive (UnsafePositive x) = x

-- | Template haskell for creating a 'Positive' at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkPositiveTH 1)
-- UnsafePositive 1
--
-- @since 0.1
#if MIN_VERSION_template_haskell(2,17,0)
mkPositiveTH :: (Integral a, Lift a, Show a) => a -> Code Q (Positive a)
#else
mkPositiveTH :: (Integral a, Lift a, Show a) => a -> Q (TExp (Positive a))
#endif
mkPositiveTH x = maybe (error err) liftTyped $ mkPositive x
  where
    err =
      "Numeric.Data.Positive.mkPositiveTH: Passed value <= 0: " <> show x

-- | Smart constructor for 'Positive'. Returns 'Nothing' if the second
-- parameter is @<= 0@.
--
-- ==== __Examples__
-- >>> mkPositive 7
-- Just (UnsafePositive 7)
--
-- >>> mkPositive 0
-- Nothing
--
-- @since 0.1
mkPositive :: (Num a, Ord a) => a -> Maybe (Positive a)
mkPositive x
  | x > 0 = Just (UnsafePositive x)
  | otherwise = Nothing

-- | Variant of 'mkPositive' that throws an error when given a value <= 0.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> unsafePositive 7
-- UnsafePositive 7
--
-- @since 0.1
unsafePositive :: (HasCallStack, Num a, Ord a, Show a) => a -> Positive a
unsafePositive x
  | x > 0 = UnsafePositive x
  | otherwise =
      error $
        "Numeric.Data.Positive.unsafePositive: Passed value <= 0: " <> show x

-- | This function is an alias for the unchecked constructor @UnsafePositive@
-- i.e. it allows us to construct a 'Positive' __without__ checking the
-- invariant. This is intended only for when we absolutely know the invariant
-- holds and a branch (i.e. 'unsafePositive') is undesirable for performance
-- reasons. Exercise extreme caution.
--
-- @since 0.1
reallyUnsafePositive :: a -> Positive a
reallyUnsafePositive = UnsafePositive

-- | Convenience function for adding a 'NonZero' proof to our 'Positive'.
--
-- ==== __Examples__
-- >>> positiveToNonZero $ unsafePositive 3
-- UnsafeNonZero (UnsafePositive 3)
--
-- @since 0.1
positiveToNonZero :: Positive a -> NonZero (Positive a)
positiveToNonZero = reallyUnsafeNonZero
