{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides the 'ModN' type for modular arithmetic.
--
-- @since 0.1.0.0
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
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
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
-- @since 0.1.0.0
type ModN :: Nat -> Type -> Type
newtype ModN n a = UnsafeModN a
  deriving stock
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Generic,
      -- | @since 0.1.0.0
      Lift,
      -- | @since 0.1.0.0
      Ord
    )
  deriving anyclass
    ( -- | @since 0.1.0.0
      NFData
    )

-- | @since 0.1.0.0
instance (KnownNat n, Show a, UpperBoundless a) => Show (ModN n a) where
  -- manual so we include the mod string
  showsPrec i (MkModN x) =
    showParen
      (i >= 11)
      (showString "MkModN " . showsPrec 11 x . showString modStr)
    where
      modStr = " (mod " <> show n' <> ")"
      n' = natVal @n Proxy

-- | Bidirectional pattern synonym for 'ModN'. Construction will apply
-- modular reduction to the parameter.
--
-- @since 0.1.0.0
pattern MkModN :: forall n a. (KnownNat n, UpperBoundless a) => a -> ModN n a
pattern MkModN x <-
  UnsafeModN x
  where
    MkModN x = mkModN x

{-# COMPLETE MkModN #-}

-- | @since 0.1.0.0
instance KnownNat n => ASemigroup (ModN n Integer) where
  type AddConstraint (ModN n Integer) = ModN n Integer
  MkModN x .+. MkModN y = MkModN $ x + y

-- | @since 0.1.0.0
instance KnownNat n => ASemigroup (ModN n Natural) where
  type AddConstraint (ModN n Natural) = ModN n Natural
  MkModN x .+. MkModN y = MkModN $ x + y

-- | @since 0.1.0.0
instance KnownNat n => AMonoid (ModN n Integer) where
  zero = MkModN 0

-- | @since 0.1.0.0
instance KnownNat n => AMonoid (ModN n Natural) where
  zero = MkModN 0

-- | @since 0.1.0.0
instance KnownNat n => AGroup (ModN n Integer) where
  type SubtractConstraint (ModN n Integer) = ModN n Integer
  MkModN x .-. MkModN y = MkModN (x - y)
  aabs = id

-- | @since 0.1.0.0
instance KnownNat n => AGroup (ModN n Natural) where
  type SubtractConstraint (ModN n Natural) = ModN n Natural
  MkModN x .-. MkModN y
    | x >= y = MkModN (x - y)
    | otherwise = MkModN (n' - y + x)
    where
      n' = natVal @n Proxy

  aabs = id

-- | @since 0.1.0.0
instance KnownNat n => MSemigroup (ModN n Integer) where
  type MultConstraint (ModN n Integer) = ModN n Integer
  MkModN x .*. MkModN y = MkModN (x * y)

-- | @since 0.1.0.0
instance KnownNat n => MSemigroup (ModN n Natural) where
  type MultConstraint (ModN n Natural) = ModN n Natural
  MkModN x .*. MkModN y = MkModN (x * y)

-- | @since 0.1.0.0
instance KnownNat n => MMonoid (ModN n Integer) where
  one = MkModN 1

-- | @since 0.1.0.0
instance KnownNat n => MMonoid (ModN n Natural) where
  one = MkModN 1

-- | @since 0.1.0.0
instance KnownNat n => Semiring (ModN n Integer)

-- | @since 0.1.0.0
instance KnownNat n => Semiring (ModN n Natural)

-- | @since 0.1.0.0
instance KnownNat n => Ring (ModN n Integer)

-- | @since 0.1.0.0
instance KnownNat n => Ring (ModN n Natural)

-- | Unwraps a 'ModN'.
--
-- @since 0.1.0.0
unModN :: ModN n a -> a
unModN (UnsafeModN x) = x

-- | Constructor for 'ModN'.
--
-- ==== __Examples__
-- >>> mkModN @5 7
-- MkModN 2 (mod 5)
--
-- >>> mkModN @10 7
-- MkModN 7 (mod 10)
--
-- @since 0.1.0.0
mkModN :: forall n a. (KnownNat n, UpperBoundless a) => a -> ModN n a
mkModN x = UnsafeModN x'
  where
    n' = fromIntegral $ natVal @n Proxy
    x' = x `mod` n'
