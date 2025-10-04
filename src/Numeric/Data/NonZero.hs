-- | Provides the 'NonZero' type for enforcing a non-zero invariant.
--
-- @since 0.1
module Numeric.Data.NonZero
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

import Data.Bifunctor (Bifunctor (first))
import Language.Haskell.TH (Code, Q)
import Language.Haskell.TH.Syntax (Lift)
import Numeric.Algebra (AMonoid, pattern NonZero, pattern Zero)
import Numeric.Data.Internal.Utils (rmatching)
import Numeric.Data.Internal.Utils qualified as Utils
import Numeric.Data.NonZero.Internal (NonZero (MkNonZero, UnsafeNonZero))
import Numeric.Data.NonZero.Internal qualified as Internal
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
-- Right (UnsafeNonZero 7)
--
-- >>> mkNonZero 0
-- Left "Numeric.Data.NonZero: Received zero"
--
-- @since 0.1
mkNonZero :: (AMonoid a, Eq a) => a -> Either String (NonZero a)
mkNonZero Zero = Left Internal.errMsg
mkNonZero (NonZero x) = Right (UnsafeNonZero x)
{-# INLINEABLE mkNonZero #-}

-- | Template-haskell version of 'mkNonZero' for creating 'NonZero'
-- at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkNonZeroTH 7)
-- UnsafeNonZero 7
--
-- @since 0.1
mkNonZeroTH :: (AMonoid a, Eq a, Lift a) => a -> Code Q (NonZero a)
mkNonZeroTH = Utils.liftErrorTH . mkNonZero
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
_MkNonZero :: (AMonoid a, Eq a) => ReversedPrism' (NonZero a) a
_MkNonZero = re (prism unNonZero g)
  where
    g x = first (const x) . mkNonZero $ x
{-# INLINEABLE _MkNonZero #-}
