-- | Provides the 'Positive' type for enforcing a positive invariant.
--
-- @since 0.1
module Numeric.Data.Positive
  ( -- * Type
    Positive (MkPositive),

    -- * Creation
    mkPositiveTH,
    mkPositive,
    Internal.unsafePositive,
    (+!),
    reallyUnsafePositive,

    -- * Elimination
    unPositive,

    -- * Functions
    positiveToNonZero,

    -- * Optics
    _MkPositive,
    rmatching,
  )
where

import GHC.Stack (HasCallStack)
import Language.Haskell.TH (Code, Q)
import Language.Haskell.TH.Syntax (Lift (liftTyped))
import Numeric.Data.NonZero (NonZero, reallyUnsafeNonZero, rmatching)
import Numeric.Data.Positive.Internal (Positive (MkPositive, UnsafePositive))
import Numeric.Data.Positive.Internal qualified as Internal
import Optics.Core
  ( ReversedPrism',
    ReversibleOptic (re),
    prism,
  )

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> :set -XPostfixOperators
-- >>> import Numeric.Data.Positive.Internal (unsafePositive)

-- | Template haskell for creating a 'Positive' at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkPositiveTH 1)
-- UnsafePositive 1
--
-- @since 0.1
mkPositiveTH :: (Integral a, Lift a, Show a) => a -> Code Q (Positive a)
mkPositiveTH x = maybe (error err) liftTyped $ mkPositive x
  where
    err = Internal.errMsg "mkPositiveTH" x
{-# INLINEABLE mkPositiveTH #-}

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
{-# INLINEABLE mkPositive #-}

-- | Postfix operator for 'Internal.unsafePositive'.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
--
-- >>> (7 +!)
-- UnsafePositive 7
--
-- @since 0.1
(+!) :: (HasCallStack, Num a, Ord a, Show a) => a -> Positive a
(+!) = Internal.unsafePositive
{-# INLINE (+!) #-}

infixl 7 +!

-- | This function is an alias for the unchecked constructor @UnsafePositive@
-- i.e. it allows us to construct a 'Positive' __without__ checking the
-- invariant. This is intended only for when we absolutely know the invariant
-- holds and a branch (i.e. 'Internal.unsafePositive') is undesirable for
-- performance reasons. Exercise extreme caution.
--
-- @since 0.1
reallyUnsafePositive :: a -> Positive a
reallyUnsafePositive = UnsafePositive
{-# INLINEABLE reallyUnsafePositive #-}

-- | Convenience function for adding a 'NonZero' proof to our 'Positive'.
--
-- ==== __Examples__
-- >>> positiveToNonZero $ unsafePositive 3
-- UnsafeNonZero (UnsafePositive 3)
--
-- @since 0.1
positiveToNonZero :: Positive a -> NonZero (Positive a)
positiveToNonZero = reallyUnsafeNonZero
{-# INLINEABLE positiveToNonZero #-}

-- | @since 0.1
unPositive :: Positive a -> a
unPositive (UnsafePositive x) = x
{-# INLINE unPositive #-}

-- | 'ReversedPrism'' that enables total elimination and partial construction.
--
-- ==== __Examples__
--
-- >>> import Optics.Core ((^.))
-- >>> pos = $$(mkPositiveTH 2)
-- >>> pos ^. _MkPositive
-- 2
--
-- >>> rmatching _MkPositive 3
-- Right (UnsafePositive 3)
--
-- >>> rmatching _MkPositive 0
-- Left 0
--
-- @since 0.1
_MkPositive :: (Num a, Ord a) => ReversedPrism' (Positive a) a
_MkPositive = re (prism unPositive g)
  where
    g x = case mkPositive x of
      Nothing -> Left x
      Just x' -> Right x'
{-# INLINEABLE _MkPositive #-}
