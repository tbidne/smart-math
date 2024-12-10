{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides the 'ModN' type for modular arithmetic.
--
-- @since 0.1
module Numeric.Data.ModN.Algebra
  ( -- * Type
    ModN (MkModN),

    -- * Creation
    Internal.mkModN,
    mkModNTH,
    Internal.unsafeModN,
    Internal.reallyUnsafeModN,

    -- * Elimination
    unModN,

    -- * Optics
    -- $optics
    _MkModN,
    rmatching,
  )
where

import Data.Bounds (MaybeUpperBounded)
import Data.Typeable (Typeable)
import GHC.TypeNats (KnownNat)
import Language.Haskell.TH.Syntax (Code, Lift (liftTyped), Q)
import Numeric.Algebra (MEuclidean)
import Numeric.Convert.Integer (FromInteger, ToInteger)
import Numeric.Data.Internal.Utils (rmatching)
import Numeric.Data.ModN.Algebra.Internal (ModN (MkModN, UnsafeModN))
import Numeric.Data.ModN.Algebra.Internal qualified as Internal
import Optics.Core (ReversedPrism', prism, re)

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> import Data.Int (Int8)
-- >>> import Numeric.Data.ModN.Algebra.Internal (mkModN)

-- | @since 0.1
unModN :: ModN n a -> a
unModN (UnsafeModN x) = x
{-# INLINE unModN #-}

-- | Template haskell for creating a 'ModN' at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkModNTH @11 7)
-- MkModN 7 (mod 11)
--
-- @since 0.1
mkModNTH ::
  forall n a.
  ( FromInteger a,
    Lift a,
    ToInteger a,
    KnownNat n,
    MaybeUpperBounded a,
    MEuclidean a,
    Typeable a
  ) =>
  a ->
  Code Q (ModN n a)
mkModNTH x = case Internal.mkModN x of
  Right y -> liftTyped y
  Left err -> error $ Internal.errMsg "mkModNTH" err
{-# INLINEABLE mkModNTH #-}

-- $optics
-- We provide a 'ReversedPrism'' '_MkModN' that allows for total
-- elimination and partial construction, along with a 'Optics.Core.LabelOptic' 'Optics.Core.Getter'
-- for @#unModN@.
--
-- ==== __Examples__
--
-- >>> :set -XOverloadedLabels
-- >>> import Optics.Core (view)
-- >>> let n = $$(mkModNTH @7 9)
-- >>> view #unModN n
-- 2

-- | 'ReversedPrism'' that enables total elimination and partial construction.
--
-- ==== __Examples__
-- >>> import Optics.Core (view)
-- >>> n = $$(mkModNTH @7 9)
-- >>> view _MkModN n
-- 2
--
-- >>> rmatching (_MkModN @7) 9
-- Right (MkModN 2 (mod 7))
--
-- >>> rmatching (_MkModN @128) (9 :: Int8)
-- Left 9
--
-- @since 0.1
_MkModN ::
  forall n a.
  ( FromInteger a,
    ToInteger a,
    KnownNat n,
    MaybeUpperBounded a,
    MEuclidean a,
    Typeable a
  ) =>
  ReversedPrism' (ModN n a) a
_MkModN = re (prism unModN g)
  where
    g x = case Internal.mkModN x of
      Left _ -> Left x
      Right x' -> Right x'
{-# INLINEABLE _MkModN #-}
