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
    unsafeLiftModP,
    unsafeLiftModP2,
    unsafeLiftModP3,

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
import Language.Haskell.TH.Syntax (Lift)
import Numeric.Algebra (MEuclidean)
import Numeric.Convert.Integer (FromInteger, ToInteger)
import Numeric.Data.Internal.Utils (rmatching)
import Numeric.Data.Internal.Utils qualified as Utils
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
  ( FromInteger a,
    KnownNat p,
    Lift a,
    MaybeUpperBounded a,
    MEuclidean a,
    ToInteger a,
    Typeable a
  ) =>
  a ->
  Code Q (ModP p a)
mkModPTH = Utils.liftErrorTH . Internal.mkModP
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
  ( FromInteger a,
    KnownNat p,
    MaybeUpperBounded a,
    MEuclidean a,
    ToInteger a,
    Typeable a
  ) =>
  ReversedPrism' (ModP p a) a
_MkModP = re (prism unModP g)
  where
    g x = case Internal.mkModP x of
      Left _ -> Left x
      Right x' -> Right x'
{-# INLINEABLE _MkModP #-}

-- | Lifts an unsafe unary function onto a 'ModP'.
--
-- @since 0.1
unsafeLiftModP ::
  ( FromInteger b,
    KnownNat p,
    MaybeUpperBounded b,
    MEuclidean b,
    ToInteger b,
    Typeable b
  ) =>
  (a -> b) ->
  ModP p a ->
  ModP p b
unsafeLiftModP f (UnsafeModP x) = Internal.unsafeModP (f x)
{-# INLINEABLE unsafeLiftModP #-}

-- | Lifts an unsafe binary function onto a 'ModP'.
--
-- @since 0.1
unsafeLiftModP2 ::
  ( FromInteger c,
    KnownNat p,
    MaybeUpperBounded c,
    MEuclidean c,
    ToInteger c,
    Typeable c
  ) =>
  (a -> b -> c) ->
  ModP p a ->
  ModP p b ->
  ModP p c
unsafeLiftModP2 f (UnsafeModP x1) (UnsafeModP x2) =
  Internal.unsafeModP (f x1 x2)
{-# INLINEABLE unsafeLiftModP2 #-}

-- | Lifts an unsafe 3-ary function onto a 'ModP'.
--
-- @since 0.1
unsafeLiftModP3 ::
  ( FromInteger d,
    KnownNat p,
    MaybeUpperBounded d,
    MEuclidean d,
    ToInteger d,
    Typeable d
  ) =>
  (a -> b -> c -> d) ->
  ModP p a ->
  ModP p b ->
  ModP p c ->
  ModP p d
unsafeLiftModP3 f (UnsafeModP x1) (UnsafeModP x2) (UnsafeModP x3) =
  Internal.unsafeModP (f x1 x2 x3)
{-# INLINEABLE unsafeLiftModP3 #-}
