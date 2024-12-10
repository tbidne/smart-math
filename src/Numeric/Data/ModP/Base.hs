-- | Provides the 'ModP' type for modular arithmetic.
--
-- @since 0.1
module Numeric.Data.ModP.Base
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
    -- $optics
    _MkModP,
    rmatching,
  )
where

import Data.Bounds (MaybeUpperBounded)
import Data.Typeable (Typeable)
import GHC.TypeNats (KnownNat)
import Language.Haskell.TH (Code, Q)
import Language.Haskell.TH.Syntax (Lift (liftTyped))
import Numeric.Data.Internal.Utils (rmatching)
import Numeric.Data.ModP.Base.Internal (ModP (MkModP, UnsafeModP))
import Numeric.Data.ModP.Base.Internal qualified as Internal
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
    MaybeUpperBounded a,
    Typeable a
  ) =>
  a ->
  Code Q (ModP p a)
mkModPTH x = case Internal.mkModP x of
  Right y -> liftTyped y
  Left err -> error $ Internal.errMsg "mkModPTH" err
{-# INLINEABLE mkModPTH #-}

-- $optics
-- We provide a 'ReversedPrism'' '_MkModP' that allows for total
-- elimination and partial construction, along with a 'Optics.Core.LabelOptic' 'Optics.Core.Getter'
-- for @#unModP@.
--
-- ==== __Examples__
--
-- >>> :set -XOverloadedLabels
-- >>> import Optics.Core (view)
-- >>> let n = $$(mkModPTH @7 9)
-- >>> view #unModP n
-- 2

-- | 'ReversedPrism'' that enables total elimination and partial construction.
--
-- ==== __Examples__
-- >>> import Optics.Core (view)
-- >>> n = $$(mkModPTH @7 9)
-- >>> view _MkModP n
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
  ( Integral a,
    KnownNat p,
    MaybeUpperBounded a,
    Typeable a
  ) =>
  ReversedPrism' (ModP p a) a
_MkModP = re (prism unModP g)
  where
    g x = case Internal.mkModP x of
      Left _ -> Left x
      Right x' -> Right x'
{-# INLINEABLE _MkModP #-}
