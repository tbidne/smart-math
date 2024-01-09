-- | Provides the 'ModP' type for modular arithmetic.
--
-- @since 0.1
module Numeric.Data.ModP
  ( -- * Type
    ModP (MkModP),

    -- * Creation
    Internal.mkModP,
    mkModPTH,
    Internal.unsafeModP,
    Internal.reallyUnsafeModP,

    -- * Elimination
    unModP,

    -- * Functions
    Internal.invert,

    -- * Optics
    _MkModP,
    rmatching,
  )
where

import Data.Bounds (UpperBoundless)
import Data.Proxy (Proxy (Proxy))
import GHC.TypeNats (KnownNat, natVal)
import Language.Haskell.TH (Code, Q)
import Language.Haskell.TH.Syntax (Lift (liftTyped))
import Numeric.Data.ModP.Internal (ModP (MkModP, UnsafeModP))
import Numeric.Data.ModP.Internal qualified as Internal
import Numeric.Data.NonZero (rmatching)
import Optics.Core (ReversedPrism', ReversibleOptic (re), prism)

-- | @since 0.1
unModP :: ModP p a -> a
unModP (UnsafeModP x) = x
{-# INLINE unModP #-}

-- | Template haskell for creating a 'ModP' at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkModPTH @11 7)
-- MkModP 7 (mod 11)
--
-- @since 0.1
mkModPTH ::
  forall p a.
  ( Integral a,
    KnownNat p,
    Lift a,
    UpperBoundless a
  ) =>
  a ->
  Code Q (ModP p a)
mkModPTH = maybe (error err) liftTyped . Internal.mkModP
  where
    err = Internal.errMsg "mkModPTH" p'
    p' = natVal @p Proxy
{-# INLINEABLE mkModPTH #-}

-- | 'ReversedPrism'' that enables total elimination and partial construction.
--
-- ==== __Examples__
--
-- >>> import Optics.Core ((^.))
-- >>> n = $$(mkModPTH @7 9)
-- >>> n ^. _MkModP
-- 2
--
-- >>> rmatching (_MkModP @7) 9
-- Right (MkModP 2 (mod 7))
--
-- >>> rmatching (_MkModP @6) 9
-- Left 9
--
-- @since 0.1
_MkModP :: forall p a. (Integral a, KnownNat p, UpperBoundless a) => ReversedPrism' (ModP p a) a
_MkModP = re (prism unModP g)
  where
    g x = case Internal.mkModP x of
      Nothing -> Left x
      Just x' -> Right x'
{-# INLINEABLE _MkModP #-}
