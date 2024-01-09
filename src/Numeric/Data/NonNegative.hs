-- | Provides the 'NonNegative' type for enforcing a nonnegative invariant.
--
-- @since 0.1
module Numeric.Data.NonNegative
  ( -- * Type
    NonNegative (MkNonNegative),

    -- * Creation
    mkNonNegativeTH,
    mkNonNegative,
    Internal.unsafeNonNegative,
    (*!),
    reallyUnsafeNonNegative,

    -- * Elimination
    unNonNegative,

    -- * Optics
    _MkNonNegative,
    rmatching,
  )
where

import GHC.Stack (HasCallStack)
import Language.Haskell.TH (Code, Q)
import Language.Haskell.TH.Syntax (Lift (liftTyped))
import Numeric.Data.NonNegative.Internal
  ( NonNegative
      ( MkNonNegative,
        UnsafeNonNegative
      ),
  )
import Numeric.Data.NonNegative.Internal qualified as Internal
import Numeric.Data.NonZero (rmatching)
import Optics.Core (ReversedPrism', ReversibleOptic (re), prism)

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> :set -XPostfixOperators

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
    err = Internal.errMsg "mkNonNegativeTH" x
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

-- | Postfix operator for 'Internal.unsafeNonNegative'.
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
(*!) = Internal.unsafeNonNegative
{-# INLINE (*!) #-}

infixl 7 *!

-- | This function is an alias for the unchecked constructor @UnsafeNonNegative@
-- i.e. it allows us to construct a 'NonNegative' __without__ checking the
-- invariant. This is intended only for when we absolutely know the invariant
-- holds and a branch (i.e. 'Internal.unsafeNonNegative') is undesirable for
-- performance reasons. Exercise extreme caution.
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
