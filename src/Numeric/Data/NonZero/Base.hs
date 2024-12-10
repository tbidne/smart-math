-- | Provides the 'NonZero' type for enforcing a non-zero invariant.
--
-- @since 0.1
module Numeric.Data.NonZero.Base
  ( -- * Type
    NonZero (MkNonZero),

    -- * Creation
    mkNonZero,
    mkNonZeroTH,
    Internal.unsafeNonZero,
    reallyUnsafeNonZero,

    -- * Elimination
    unNonZero,

    -- * Optics
    -- $optics
    _MkNonZero,
    rmatching,
  )
where

import Language.Haskell.TH (Code, Q)
import Language.Haskell.TH.Syntax (Lift (liftTyped))
import Numeric.Data.Internal.Utils (rmatching)
import Numeric.Data.NonZero.Base.Internal (NonZero (MkNonZero, UnsafeNonZero))
import Numeric.Data.NonZero.Base.Internal qualified as Internal
import Optics.Core (ReversedPrism', prism, re)

-- $setup
-- >>> :set -XTemplateHaskell

-- | @since 0.1
unNonZero :: NonZero a -> a
unNonZero (UnsafeNonZero x) = x

-- | Smart constructor for 'NonZero'.
--
-- ==== __Examples__
-- >>> mkNonZero 7
-- Just (UnsafeNonZero 7)
--
-- >>> mkNonZero 0
-- Nothing
--
-- @since 0.1
mkNonZero :: (Eq a, Num a) => a -> Maybe (NonZero a)
mkNonZero x
  | x == 0 = Nothing
  | otherwise = Just (UnsafeNonZero x)
{-# INLINEABLE mkNonZero #-}

-- | Template-haskell version of 'mkNonZero' for creating 'NonZero'
-- at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkNonZeroTH 7)
-- UnsafeNonZero 7
--
-- @since 0.1
mkNonZeroTH :: (Eq a, Lift a, Num a) => a -> Code Q (NonZero a)
mkNonZeroTH x
  | x == 0 = error $ Internal.errMsg "mkNonZeroTH"
  | otherwise = liftTyped (UnsafeNonZero x)
{-# INLINEABLE mkNonZeroTH #-}

-- | This function is an alias for the unchecked constructor @UnsafeNonZero@
-- i.e. it allows us to construct a 'NonZero' __without__ checking the
-- invariant. This is intended only for when we absolutely know the invariant
-- holds and a branch (i.e. 'Internal.unsafeNonZero') is undesirable for
-- performance reasons. Exercise extreme caution.
--
-- @since 0.1
reallyUnsafeNonZero :: a -> NonZero a
reallyUnsafeNonZero = UnsafeNonZero
{-# INLINE reallyUnsafeNonZero #-}

-- $optics
-- We provide a 'ReversedPrism'' '_MkNonZero' that allows for total
-- elimination and partial construction, along with a 'Optics.Core.LabelOptic' 'Optics.Core.Getter'
-- for @#unNonZero@.
--
-- ==== __Examples__
--
-- >>> :set -XOverloadedLabels
-- >>> import Optics.Core (view)
-- >>> let n = $$(mkNonZeroTH 7)
-- >>> view #unNonZero n
-- 7

-- | 'ReversedPrism'' that enables total elimination and partial construction.
--
-- ==== __Examples__
-- >>> import Optics.Core (view)
-- >>> nz = $$(mkNonZeroTH 7)
-- >>> view _MkNonZero nz
-- 7
--
-- >>> rmatching _MkNonZero 3
-- Right (UnsafeNonZero 3)
--
-- >>> rmatching _MkNonZero 0
-- Left 0
--
-- @since 0.1
_MkNonZero :: (Eq a, Num a) => ReversedPrism' (NonZero a) a
_MkNonZero = re (prism f g)
  where
    f = unNonZero
    g x = case mkNonZero x of
      Nothing -> Left x
      Just x' -> Right x'
{-# INLINEABLE _MkNonZero #-}
