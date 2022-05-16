{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'ModP' type for modular arithmetic.
--
-- @since 0.1
module Numeric.Data.ModP
  ( -- * Type
    ModP (MkModP),

    -- * Creation
    mkModP,
    mkModPTH,
    unsafeModP,
    reallyUnsafeModP,

    -- * Elimination
    unModP,

    -- * Functions
    invert,
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
import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat, Nat, natVal)
#if MIN_VERSION_template_haskell(2, 17, 0)
import Language.Haskell.TH (Code, Q)
#else
import Language.Haskell.TH (Q, TExp)
#endif
import Language.Haskell.TH.Syntax (Lift (..))
import Numeric.Algebra.Additive.AGroup (AGroup (..))
import Numeric.Algebra.Additive.AMonoid (AMonoid (..))
import Numeric.Algebra.Additive.ASemigroup (ASemigroup (..))
import Numeric.Algebra.Field (Field)
import Numeric.Algebra.Multiplicative.MGroup (MGroup (..))
import Numeric.Algebra.Multiplicative.MMonoid (MMonoid (..))
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup (..))
import Numeric.Algebra.Ring (Ring)
import Numeric.Algebra.Semifield (Semifield)
import Numeric.Algebra.Semiring (Semiring)
import Numeric.Class.Boundless (UpperBoundless)
import Numeric.Class.Literal (NumLiteral (..))
import Numeric.Data.ModP.Internal (MaybePrime (..), Modulus (..))
import Numeric.Data.ModP.Internal qualified as ModPI
import Numeric.Data.NonZero (NonZero (..))
import Optics.Core (A_Lens, LabelOptic (..), lens)
#if MIN_VERSION_prettyprinter(1, 7, 1)
import Prettyprinter (Pretty (..), (<+>))
#endif

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> import Numeric.Algebra.Multiplicative.MGroup (unsafeAMonoidNonZero)

-- | Newtype wrapper that represents \( \mathbb{Z}/p\mathbb{Z} \) for prime @p@.
-- 'ModP' is a:
--
-- * 'Numeric.Algebra.Additive.ASemigroup.ASemigroup'
-- * 'Numeric.Algebra.Additive.AMonoid.AMonoid'
-- * 'Numeric.Algebra.Additive.AGroup.AGroup'
-- * 'Numeric.Algebra.Multiplicative.MSemigroup.MSemigroup'
-- * 'Numeric.Algebra.Multiplicative.MMonoid.MMonoid'
-- * 'Numeric.Algebra.Multiplicative.MGroup.MGroup'
-- * 'Numeric.Algebra.Semiring.Semiring'
-- * 'Numeric.Algebra.Ring.Ring'
-- * 'Numeric.Algebra.Field.Field'
--
-- @since 0.1
type ModP :: Nat -> Type -> Type
newtype ModP p a = UnsafeModP
  { -- | @since 0.1
    unModP :: a
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
    KnownNat p,
    UpperBoundless b
  ) =>
  LabelOptic "unModP" k (ModP p a) (ModP p b) x y
  where
  labelOptic = lens unModP (\_ x -> reallyUnsafeModP x)
  {-# INLINEABLE labelOptic #-}

-- | @since 0.1
instance (KnownNat p, Show a, UpperBoundless a) => Show (ModP p a) where
  -- manual so we show "MkModP" instead of "UnsafeModP"
  showsPrec i (UnsafeModP x) =
    showParen
      (i >= 11)
      (showString "MkModP " . showsPrec 11 x . showString modStr)
    where
      modStr = " (mod " <> show p' <> ")"
      p' = natVal @p Proxy
  {-# INLINEABLE showsPrec #-}

-- | Bidirectional pattern synonym for 'ModP'. Construction fails when @p@ is
-- not prime. Note that because this performs a primality check, construction
-- is __not__ \(O(1)\). See 'mkModP' for details. For much faster construction,
-- when you are /sure/ @p@ is prime, see 'reallyUnsafeModP'.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> MkModP @7 12
-- MkModP 5 (mod 7)
--
-- @since 0.1
pattern MkModP :: (HasCallStack, KnownNat p, UpperBoundless a) => a -> ModP p a
pattern MkModP x <-
  UnsafeModP x
  where
    MkModP x = unsafeModP x

{-# COMPLETE MkModP #-}

-- | @since 0.1
instance (KnownNat p, Pretty a) => Pretty (ModP p a) where
  pretty (UnsafeModP x) =
    pretty x
      <+> pretty @String "(mod"
      <+> pretty p'
      <> pretty @String ")"
    where
      p' = natVal @p Proxy
  {-# INLINEABLE pretty #-}

-- | @since 0.1
instance KnownNat p => ASemigroup (ModP p Integer) where
  MkModP x .+. MkModP y = unsafeModP $ x + y
  {-# INLINEABLE (.+.) #-}

-- | @since 0.1
instance KnownNat p => ASemigroup (ModP p Natural) where
  MkModP x .+. MkModP y = unsafeModP $ x + y
  {-# INLINEABLE (.+.) #-}

-- | @since 0.1
instance KnownNat p => AMonoid (ModP p Integer) where
  zero = MkModP 0
  {-# INLINEABLE zero #-}

-- | @since 0.1
instance KnownNat p => AMonoid (ModP p Natural) where
  zero = MkModP 0
  {-# INLINEABLE zero #-}

-- | @since 0.1
instance KnownNat p => AGroup (ModP p Integer) where
  MkModP x .-. MkModP y = unsafeModP (x - y)
  {-# INLINEABLE (.-.) #-}

-- | @since 0.1
instance KnownNat p => AGroup (ModP p Natural) where
  MkModP x .-. MkModP y
    | x >= y = unsafeModP (x - y)
    | otherwise = unsafeModP (p' - y + x)
    where
      p' = natVal @p Proxy
  {-# INLINEABLE (.-.) #-}

-- | @since 0.1
instance KnownNat p => MSemigroup (ModP p Integer) where
  MkModP x .*. MkModP y = unsafeModP (x * y)
  {-# INLINEABLE (.*.) #-}

-- | @since 0.1
instance KnownNat p => MSemigroup (ModP p Natural) where
  MkModP x .*. MkModP y = unsafeModP (x * y)
  {-# INLINEABLE (.*.) #-}

-- | @since 0.1
instance KnownNat p => MMonoid (ModP p Integer) where
  one = MkModP 1
  {-# INLINEABLE one #-}

-- | @since 0.1
instance KnownNat p => MMonoid (ModP p Natural) where
  one = MkModP 1
  {-# INLINEABLE one #-}

-- | @since 0.1
instance KnownNat p => MGroup (ModP p Integer) where
  x .%. d = x .*. invert d
  {-# INLINEABLE (.%.) #-}

-- | @since 0.1
instance KnownNat p => MGroup (ModP p Natural) where
  x .%. d = x .*. invert d
  {-# INLINEABLE (.%.) #-}

-- | @since 0.1
instance KnownNat p => Semiring (ModP p Integer)

-- | @since 0.1
instance KnownNat p => Semiring (ModP p Natural)

-- | @since 0.1
instance KnownNat p => Ring (ModP p Integer)

-- | @since 0.1
instance KnownNat p => Ring (ModP p Natural)

-- | @since 0.1
instance KnownNat p => Semifield (ModP p Integer)

-- | @since 0.1
instance KnownNat p => Semifield (ModP p Natural)

-- | @since 0.1
instance KnownNat p => Field (ModP p Integer)

-- | @since 0.1
instance KnownNat p => Field (ModP p Natural)

-- | @since 0.1
instance (KnownNat p, UpperBoundless a) => NumLiteral (ModP p a) where
  fromLit = MkModP . fromInteger
  {-# INLINEABLE fromLit #-}

-- | Constructor for 'ModP'. Fails if @p@ is not prime. This uses the
-- Miller-Rabin primality test, which has complexity \(O(k \log^3 p)\), and we
-- take \(k = 100\). See
-- [wikipedia](https://en.wikipedia.org/wiki/Miller-Rabin_primality_test#Complexity)
-- for more details.
--
-- ==== __Examples__
-- >>> mkModP @5 7
-- Just (MkModP 2 (mod 5))
--
-- >>> mkModP @10 7
-- Nothing
--
-- @since 0.1
mkModP :: forall p a. (KnownNat p, UpperBoundless a) => a -> Maybe (ModP p a)
mkModP x = case ModPI.isPrime p' of
  Composite -> Nothing
  ProbablyPrime -> Just $ UnsafeModP x'
  where
    p' = toInteger $ natVal @p Proxy
    x' = x `mod` toUpperBoundless p'
{-# INLINEABLE mkModP #-}

-- | Template haskell for creating a 'ModP' at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkModPTH @11 7)
-- MkModP 7 (mod 11)
--
-- @since 0.1
#if MIN_VERSION_template_haskell(2,17,0)
mkModPTH :: forall p a. (KnownNat p, Lift a, UpperBoundless a) => a -> Code Q (ModP p a)
#else
mkModPTH :: forall p a. (KnownNat p, Lift a, UpperBoundless a) => a -> Q (TExp (ModP p a))
#endif
mkModPTH = maybe (error err) liftTyped . mkModP
  where
    err =
      "Numeric.Data.ModP.mkModPTH: Passed non-prime: "
        <> show p'
    p' = natVal @p Proxy
{-# INLINEABLE mkModPTH #-}

-- | Variant of 'mkModP' that throws an error when given a non-prime.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> unsafeModP @7 12
-- MkModP 5 (mod 7)
--
-- @since 0.1
unsafeModP :: forall p a. (HasCallStack, KnownNat p, UpperBoundless a) => a -> ModP p a
unsafeModP x = case mkModP x of
  Just mp -> mp
  Nothing ->
    error $
      "Numeric.Data.ModP.unsafeModP: Passed non-prime: " <> show p'
  where
    p' = natVal @p Proxy
{-# INLINEABLE unsafeModP #-}

-- | This function reduces the argument modulo @p@ but does __not__ check
-- that @p@ is prime. Note that the correct behavior of some functionality
-- (e.g. division) is reliant on primality, so this is dangerous. This is
-- intended only for when we absolutely know @p@ is prime and the check
-- is undesirable for performance reasons. Exercise extreme caution.
--
-- @since 0.1
reallyUnsafeModP :: forall p a. (KnownNat p, UpperBoundless a) => a -> ModP p a
reallyUnsafeModP = UnsafeModP . (`mod` p')
  where
    p' = fromIntegral $ natVal @p Proxy
{-# INLINEABLE reallyUnsafeModP #-}

-- | Given non-zero \(d\), returns the inverse i.e. finds \(e\) s.t.
--
-- \[
-- de \equiv 1 \pmod p.
-- \]
--
-- ==== __Examples__
--
-- >>> invert $ unsafeAMonoidNonZero $ MkModP @7 5
-- MkModP 3 (mod 7)
--
-- >>> invert $ unsafeAMonoidNonZero $ MkModP @19 12
-- MkModP 8 (mod 19)
--
-- @since 0.1
invert :: forall p a. (KnownNat p, UpperBoundless a) => NonZero (ModP p a) -> ModP p a
invert (MkNonZero (UnsafeModP d)) = reallyUnsafeModP $ toUpperBoundless $ ModPI.findInverse d' p'
  where
    p' = MkModulus $ fromIntegral $ natVal @p Proxy
    d' = toInteger d
{-# INLINEABLE invert #-}

-- Note: obviously this is partial when @a@ is 'Natural'
toUpperBoundless :: UpperBoundless a => Integer -> a
toUpperBoundless = fromIntegral
{-# INLINEABLE toUpperBoundless #-}
