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

import Data.Bounds (AnyUpperBounded)
import Data.Typeable (Typeable)
import GHC.TypeNats (KnownNat)
import Language.Haskell.TH (Code, Q)
import Language.Haskell.TH.Syntax (Lift (liftTyped))
import Numeric.Data.Internal.Utils (rmatching)
import Numeric.Data.ModP.Internal (ModP (MkModP, UnsafeModP))
import Numeric.Data.ModP.Internal qualified as Internal
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
  ( AnyUpperBounded a,
    Integral a,
    KnownNat p,
    Lift a,
    Typeable a
  ) =>
  a ->
  Code Q (ModP p a)
mkModPTH x = case Internal.mkModP x of
  Right y -> liftTyped y
  Left err -> error $ Internal.errMsg "mkModPTH" err
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
_MkModP ::
  forall p a.
  ( AnyUpperBounded a,
    Integral a,
    KnownNat p,
    Typeable a
  ) =>
  ReversedPrism' (ModP p a) a
_MkModP = re (prism unModP g)
  where
    g x = case Internal.mkModP x of
      Left _ -> Left x
      Right x' -> Right x'
{-# INLINEABLE _MkModP #-}
