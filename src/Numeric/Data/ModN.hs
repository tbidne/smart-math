{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides the 'ModN' type for modular arithmetic.
--
-- @since 0.1
module Numeric.Data.ModN
  ( -- * Type
    ModN (MkModN),

    -- * Creation
    mkModN,

    -- * Elimination
    unModN,
  )
where

import Control.DeepSeq (NFData)
import Data.Data (Data)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
#if !MIN_VERSION_prettyprinter(1, 7, 1)
import Data.Text.Prettyprint.Doc (Pretty (..), (<+>))
#endif
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import GHC.TypeNats (KnownNat, Nat, natVal)
import Language.Haskell.TH.Syntax (Lift)
import Numeric.Algebra.Additive.AGroup (AGroup (..))
import Numeric.Algebra.Additive.AMonoid (AMonoid (..))
import Numeric.Algebra.Additive.ASemigroup (ASemigroup (..))
import Numeric.Algebra.Multiplicative.MMonoid (MMonoid (..))
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup (..))
import Numeric.Algebra.Ring (Ring)
import Numeric.Algebra.Semiring (Semiring)
import Numeric.Class.Boundless (UpperBoundless)
import Numeric.Class.Literal (NumLiteral (..))
import Optics.Core (A_Lens, LabelOptic (..), lens)
#if MIN_VERSION_prettyprinter(1, 7, 1)
import Prettyprinter (Pretty (..), (<+>))
#endif

-- $setup
-- >>> :set -XTemplateHaskell

-- | Newtype wrapper that represents \( \mathbb{Z}/n\mathbb{Z} \).
-- 'ModN' is a:
--
-- * 'Numeric.Algebra.Additive.ASemigroup.ASemigroup'
-- * 'Numeric.Algebra.Additive.AMonoid.AMonoid'
-- * 'Numeric.Algebra.Additive.AGroup.AGroup'
-- * 'Numeric.Algebra.Multiplicative.MSemigroup.MSemigroup'
-- * 'Numeric.Algebra.Multiplicative.MMonoid.MMonoid'
-- * 'Numeric.Algebra.Semiring.Semiring'
-- * 'Numeric.Algebra.Ring.Ring'
--
-- @since 0.1
type ModN :: Nat -> Type -> Type
newtype ModN n a = UnsafeModN
  { -- | @since 0.1
    unModN :: a
  }
  deriving stock
    ( -- | @since 0.1
      Data,
      -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Lift,
      -- | @since 0.1
      Ord
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance
  ( k ~ A_Lens,
    a ~ x,
    b ~ y,
    KnownNat n,
    UpperBoundless b
  ) =>
  LabelOptic "unModN" k (ModN n a) (ModN n b) x y
  where
  labelOptic = lens unModN (\_ x -> mkModN x)
  {-# INLINEABLE labelOptic #-}

-- | @since 0.1
instance (KnownNat n, Show a, UpperBoundless a) => Show (ModN n a) where
  -- manual so we include the mod string
  showsPrec i (MkModN x) =
    showParen
      (i >= 11)
      (showString "MkModN " . showsPrec 11 x . showString modStr)
    where
      modStr = " (mod " <> show n' <> ")"
      n' = natVal @n Proxy
  {-# INLINEABLE showsPrec #-}

-- | Bidirectional pattern synonym for 'ModN'. Construction will apply
-- modular reduction to the parameter.
--
-- @since 0.1
pattern MkModN :: forall n a. (KnownNat n, UpperBoundless a) => a -> ModN n a
pattern MkModN x <-
  UnsafeModN x
  where
    MkModN x = mkModN x

{-# COMPLETE MkModN #-}

-- | @since 0.1
instance (KnownNat n, Pretty a) => Pretty (ModN n a) where
  pretty (UnsafeModN x) =
    pretty x
      <+> pretty @String "(mod"
      <+> pretty n'
        <> pretty @String ")"
    where
      n' = natVal @n Proxy
  {-# INLINEABLE pretty #-}

-- | @since 0.1
instance KnownNat n => ASemigroup (ModN n Integer) where
  MkModN x .+. MkModN y = MkModN $ x + y
  {-# INLINEABLE (.+.) #-}

-- | @since 0.1
instance KnownNat n => ASemigroup (ModN n Natural) where
  MkModN x .+. MkModN y = MkModN $ x + y
  {-# INLINEABLE (.+.) #-}

-- | @since 0.1
instance KnownNat n => AMonoid (ModN n Integer) where
  zero = MkModN 0
  {-# INLINEABLE zero #-}

-- | @since 0.1
instance KnownNat n => AMonoid (ModN n Natural) where
  zero = MkModN 0
  {-# INLINEABLE zero #-}

-- | @since 0.1
instance KnownNat n => AGroup (ModN n Integer) where
  MkModN x .-. MkModN y = MkModN (x - y)
  {-# INLINEABLE (.-.) #-}

-- | @since 0.1
instance KnownNat n => AGroup (ModN n Natural) where
  MkModN x .-. MkModN y
    | x >= y = MkModN (x - y)
    | otherwise = MkModN (n' - y + x)
    where
      n' = natVal @n Proxy
  {-# INLINEABLE (.-.) #-}

-- | @since 0.1
instance KnownNat n => MSemigroup (ModN n Integer) where
  MkModN x .*. MkModN y = MkModN (x * y)
  {-# INLINEABLE (.*.) #-}

-- | @since 0.1
instance KnownNat n => MSemigroup (ModN n Natural) where
  MkModN x .*. MkModN y = MkModN (x * y)
  {-# INLINEABLE (.*.) #-}

-- | @since 0.1
instance KnownNat n => MMonoid (ModN n Integer) where
  one = MkModN 1
  {-# INLINEABLE one #-}

-- | @since 0.1
instance KnownNat n => MMonoid (ModN n Natural) where
  one = MkModN 1
  {-# INLINEABLE one #-}

-- | @since 0.1
instance KnownNat n => Semiring (ModN n Integer)

-- | @since 0.1
instance KnownNat n => Semiring (ModN n Natural)

-- | @since 0.1
instance KnownNat n => Ring (ModN n Integer)

-- | @since 0.1
instance KnownNat n => Ring (ModN n Natural)

-- | @since 0.1
instance (KnownNat n, UpperBoundless a) => NumLiteral (ModN n a) where
  fromLit = MkModN . fromInteger
  {-# INLINEABLE fromLit #-}

-- | Constructor for 'ModN'.
--
-- ==== __Examples__
-- >>> mkModN @5 7
-- MkModN 2 (mod 5)
--
-- >>> mkModN @10 7
-- MkModN 7 (mod 10)
--
-- @since 0.1
mkModN :: forall n a. (KnownNat n, UpperBoundless a) => a -> ModN n a
mkModN x = UnsafeModN x'
  where
    n' = fromIntegral $ natVal @n Proxy
    x' = x `mod` n'
{-# INLINEABLE mkModN #-}
