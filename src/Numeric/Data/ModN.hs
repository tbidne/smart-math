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

    -- * Optics
    _MkModN,
  )
where

import Control.DeepSeq (NFData)
import Data.Bounds (LowerBounded, UpperBounded, UpperBoundless)
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Text.Display (Display (displayBuilder))
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import GHC.TypeNats (KnownNat, Nat, natVal)
import Language.Haskell.TH.Syntax (Lift)
import Numeric.Algebra.Additive.AGroup (AGroup ((.-.)))
import Numeric.Algebra.Additive.AMonoid (AMonoid (zero))
import Numeric.Algebra.Additive.ASemigroup (ASemigroup ((.+.)))
import Numeric.Algebra.Multiplicative.MMonoid (MMonoid (one))
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup ((.*.)))
import Numeric.Algebra.Ring (Ring)
import Numeric.Algebra.Semiring (Semiring)
import Numeric.Literal.Integer (FromInteger (afromInteger))
import Optics.Core (Lens', lens)

-- $setup
-- >>> :set -XTemplateHaskell

-- | Newtype wrapper that represents \( \mathbb{Z}/n\mathbb{Z} \).
-- 'ModN' is a 'Numeric.Algebra.Ring.Ring' i.e. supports addition, subtraction,
-- and multiplication.
--
-- @since 0.1
type ModN :: Nat -> Type -> Type
newtype ModN n a = UnsafeModN a
  deriving stock
    ( -- | @since 0.1
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
      LowerBounded,
      -- | @since 0.1
      NFData,
      -- | @since 0.1
      UpperBounded
    )

-- | @since 0.1
unModN :: ModN n a -> a
unModN (UnsafeModN x) = x
{-# INLINE unModN #-}

-- | @since 0.1
instance (Integral a, KnownNat n, Show a, UpperBoundless a) => Show (ModN n a) where
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
pattern MkModN :: forall n a. (Integral a, KnownNat n, UpperBoundless a) => a -> ModN n a
pattern MkModN x <-
  UnsafeModN x
  where
    MkModN x = mkModN x

{-# COMPLETE MkModN #-}

-- | @since 0.1
instance (KnownNat n, Num a) => Bounded (ModN n a) where
  minBound = UnsafeModN 0
  maxBound = UnsafeModN $ fromIntegral (natVal @n Proxy - 1)
  {-# INLINEABLE minBound #-}
  {-# INLINEABLE maxBound #-}

-- | @since 0.1
instance (KnownNat n, Show a) => Display (ModN n a) where
  displayBuilder (UnsafeModN x) =
    mconcat
      [ displayBuilder $ show x,
        displayBuilder @String " (mod ",
        displayBuilder $ show n',
        displayBuilder @String ")"
      ]
    where
      n' = natVal @n Proxy

-- | @since 0.1
instance (KnownNat n) => ASemigroup (ModN n Integer) where
  MkModN x .+. MkModN y = MkModN $ x + y
  {-# INLINEABLE (.+.) #-}

-- | @since 0.1
instance (KnownNat n) => ASemigroup (ModN n Natural) where
  MkModN x .+. MkModN y = MkModN $ x + y
  {-# INLINEABLE (.+.) #-}

-- | @since 0.1
instance (KnownNat n) => AMonoid (ModN n Integer) where
  zero = MkModN 0
  {-# INLINEABLE zero #-}

-- | @since 0.1
instance (KnownNat n) => AMonoid (ModN n Natural) where
  zero = MkModN 0
  {-# INLINEABLE zero #-}

-- | @since 0.1
instance (KnownNat n) => AGroup (ModN n Integer) where
  MkModN x .-. MkModN y = MkModN (x - y)
  {-# INLINEABLE (.-.) #-}

-- | @since 0.1
instance (KnownNat n) => AGroup (ModN n Natural) where
  MkModN x .-. MkModN y
    | x >= y = MkModN (x - y)
    | otherwise = MkModN (n' - y + x)
    where
      n' = natVal @n Proxy
  {-# INLINEABLE (.-.) #-}

-- | @since 0.1
instance (KnownNat n) => MSemigroup (ModN n Integer) where
  MkModN x .*. MkModN y = MkModN (x * y)
  {-# INLINEABLE (.*.) #-}

-- | @since 0.1
instance (KnownNat n) => MSemigroup (ModN n Natural) where
  MkModN x .*. MkModN y = MkModN (x * y)
  {-# INLINEABLE (.*.) #-}

-- | @since 0.1
instance (KnownNat n) => MMonoid (ModN n Integer) where
  one = MkModN 1
  {-# INLINEABLE one #-}

-- | @since 0.1
instance (KnownNat n) => MMonoid (ModN n Natural) where
  one = MkModN 1
  {-# INLINEABLE one #-}

-- | @since 0.1
instance (KnownNat n) => Semiring (ModN n Integer)

-- | @since 0.1
instance (KnownNat n) => Semiring (ModN n Natural)

-- | @since 0.1
instance (KnownNat n) => Ring (ModN n Integer)

-- | @since 0.1
instance (KnownNat n) => Ring (ModN n Natural)

-- | @since 0.1
instance (Integral a, KnownNat n, UpperBoundless a) => FromInteger (ModN n a) where
  afromInteger = MkModN . fromInteger
  {-# INLINEABLE afromInteger #-}

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
mkModN :: forall n a. (Integral a, KnownNat n, UpperBoundless a) => a -> ModN n a
mkModN x = UnsafeModN x'
  where
    n' = fromIntegral $ natVal @n Proxy
    x' = x `mod` n'
{-# INLINEABLE mkModN #-}

-- | 'Lens'' for 'ModN'. Despite being a 'Lens', we use prism/iso syntax for
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
_MkModN :: forall n a. (Integral a, KnownNat n, Ord a, UpperBoundless a) => Lens' (ModN n a) a
_MkModN = lens unModN (\_ x -> mkModN x)
{-# INLINEABLE _MkModN #-}
