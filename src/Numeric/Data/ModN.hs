{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides the 'ModN' type for modular arithmetic.
--
-- @since 0.1
module Numeric.Data.ModN
  ( -- * Type
    ModN (MkModN),

    -- * Creation
    Internal.mkModN,

    -- * Elimination
    unModN,

    -- * Optics
    _MkModN,
  )
where

import Data.Bounds (UpperBoundless)
import GHC.TypeNats (KnownNat)
import Numeric.Data.ModN.Internal (ModN (MkModN, UnsafeModN))
import Numeric.Data.ModN.Internal qualified as Internal
import Optics.Core (Lens', lens)

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> import Numeric.Data.ModN.Internal (mkModN)

-- | @since 0.1
unModN :: ModN n a -> a
unModN (UnsafeModN x) = x
{-# INLINE unModN #-}

-- | 'Lens'' for 'ModN'. Despite being a 'Lens'', we use prism/iso syntax for
-- consistency with other optics in this package and to witness that
-- 'ModN' is _nearly_ an Iso.
--
-- ==== __Examples__
--
-- >>> import Optics.Core ((^.), (.~))
-- >>> n = mkModN @7 9
-- >>> n ^. _MkModN
-- 2
--
-- >>> (_MkModN .~ 2) n
-- MkModN 2 (mod 7)
--
-- @since 0.1
_MkModN ::
  forall n a.
  ( Integral a,
    KnownNat n,
    Ord a,
    UpperBoundless a
  ) =>
  Lens' (ModN n a) a
_MkModN = lens unModN (\_ x -> Internal.mkModN x)
{-# INLINEABLE _MkModN #-}
