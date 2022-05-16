{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'NonNegative' type for enforcing a nonnegative invariant.
--
-- @since 0.1
module Numeric.Data.NonNegative
  ( -- * Type
    NonNegative (MkNonNegative),

    -- * Creation
    mkNonNegativeTH,
    mkNonNegative,
    unsafeNonNegative,
    reallyUnsafeNonNegative,

    -- * Elimination
    unNonNegative,
  )
where

import Control.DeepSeq (NFData)
import Data.Bifunctor (Bifunctor (..))
import Data.Data (Data)
import Data.Kind (Type)
#if !MIN_VERSION_prettyprinter(1, 7, 1)
import Data.Text.Prettyprint.Doc (Pretty (..))
#endif
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
#if MIN_VERSION_template_haskell(2, 17, 0)
import Language.Haskell.TH (Code, Q)
#else
import Language.Haskell.TH (Q, TExp)
#endif
import Language.Haskell.TH.Syntax (Lift (..))
import Numeric.Algebra.Additive.AMonoid (AMonoid (..))
import Numeric.Algebra.Additive.ASemigroup (ASemigroup (..))
import Numeric.Algebra.Multiplicative.MGroup (MGroup (..), MGroupIntegral (..))
import Numeric.Algebra.Multiplicative.MMonoid (MMonoid (..))
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup (..))
import Numeric.Algebra.Normed (Normed (..))
import Numeric.Algebra.Semifield (Semifield)
import Numeric.Algebra.Semiring (Semiring)
import Numeric.Class.Division (Division (..))
import Numeric.Class.Literal (NumLiteral (..))
import Numeric.Data.NonZero (NonZero (..))
import Optics.Core (A_Getter, LabelOptic (..), to)
#if MIN_VERSION_prettyprinter(1, 7, 1)
import Prettyprinter (Pretty (..))
#endif

-- $setup
-- >>> :set -XTemplateHaskell

-- | Newtype wrapper that attaches a 'NonNegative' invariant to some @a@.
-- 'NonNegative' is a:
--
-- * 'Numeric.Algebra.Additive.ASemigroup.ASemigroup'
-- * 'Numeric.Algebra.Additive.AMonoid.AMonoid'
-- * 'Numeric.Algebra.Multiplicative.MSemigroup.MSemigroup'
-- * 'Numeric.Algebra.Multiplicative.MMonoid.MMonoid'
-- * 'Numeric.Algebra.Multiplicative.MGroup.MGroup'
-- * 'Numeric.Algebra.Multiplicative.MGroup.MGroupIntegral'
-- * 'Numeric.Algebra.Semiring.Semiring'
--
-- @since 0.1
type NonNegative :: Type -> Type
newtype NonNegative a = UnsafeNonNegative
  { -- | @since 0.1
    unNonNegative :: a
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
      Ord,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | Bidirectional pattern synonym for 'NonNegative'. Construction fails when
-- the given value is positive.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> MkNonNegative 0
-- UnsafeNonNegative {unNonNegative = 0}
--
-- @since 0.1
pattern MkNonNegative :: (Num a, Ord a, Show a) => a -> NonNegative a
pattern MkNonNegative x <-
  UnsafeNonNegative x
  where
    MkNonNegative x = unsafeNonNegative x

{-# COMPLETE MkNonNegative #-}

-- | @since 0.1
instance (k ~ A_Getter, a ~ n) => LabelOptic "unNonNegative" k (NonNegative n) (NonNegative n) a a where
  labelOptic = to unNonNegative
  {-# INLINEABLE labelOptic #-}

-- | @since 0.1
instance Pretty a => Pretty (NonNegative a) where
  pretty (UnsafeNonNegative x) = pretty x
  {-# INLINEABLE pretty #-}

-- | @since 0.1
instance (Eq a, Num a) => ASemigroup (NonNegative a) where
  UnsafeNonNegative x .+. UnsafeNonNegative y = UnsafeNonNegative $ x + y
  {-# INLINEABLE (.+.) #-}

-- | @since 0.1
instance (Eq a, Num a) => AMonoid (NonNegative a) where
  zero = UnsafeNonNegative 0
  {-# INLINEABLE zero #-}

-- | @since 0.1
instance (Eq a, Num a) => MSemigroup (NonNegative a) where
  UnsafeNonNegative x .*. UnsafeNonNegative y = UnsafeNonNegative $ x * y
  {-# INLINEABLE (.*.) #-}

-- | @since 0.1
instance (Eq a, Num a) => MMonoid (NonNegative a) where
  one = UnsafeNonNegative 1
  {-# INLINEABLE one #-}

-- | @since 0.1
instance (Eq a, Division a, Num a) => MGroup (NonNegative a) where
  UnsafeNonNegative x .%. MkNonZero (UnsafeNonNegative d) = UnsafeNonNegative $ x `divide` d
  {-# INLINEABLE (.%.) #-}

-- | @since 0.1
instance (Division a, Integral a) => MGroupIntegral (NonNegative a) where
  type ModResult (NonNegative a) = NonNegative a
  UnsafeNonNegative x `mdivMod` MkNonZero (UnsafeNonNegative d) =
    bimap UnsafeNonNegative UnsafeNonNegative $ x `divMod` d
  {-# INLINEABLE mdivMod #-}

-- | @since 0.1
instance Normed (NonNegative a) where
  norm = id
  {-# INLINEABLE norm #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance (Num a, Ord a, Show a) => NumLiteral (NonNegative a) where
  fromLit = unsafeNonNegative . fromInteger
  {-# INLINEABLE fromLit #-}

-- | @since 0.1
instance (Eq a, Num a) => Semiring (NonNegative a)

-- | @since 0.1
instance (Division a, Eq a, Num a) => Semifield (NonNegative a)

-- | Template haskell for creating a 'NonNegative' at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkNonNegativeTH 1)
-- UnsafeNonNegative {unNonNegative = 1}
--
-- @since 0.1
#if MIN_VERSION_template_haskell(2,17,0)
mkNonNegativeTH :: (Integral a, Lift a, Show a) => a -> Code Q (NonNegative a)
#else
mkNonNegativeTH :: (Integral a, Lift a, Show a) => a -> Q (TExp (NonNegative a))
#endif
mkNonNegativeTH x = maybe (error err) liftTyped $ mkNonNegative x
  where
    err =
      "Numeric.Data.NonNegative.mkNonNegativeTH: Passed value < 0: " <> show x
{-# INLINEABLE mkNonNegativeTH #-}

-- | Smart constructor for 'NonNegative'. Returns 'Nothing' if the second
-- parameter is @< 0@.
--
-- ==== __Examples__
-- >>> mkNonNegative 0
-- Just (UnsafeNonNegative {unNonNegative = 0})
--
-- >>> mkNonNegative (-2)
-- Nothing
--
-- @since 0.1
mkNonNegative :: (Num a, Ord a) => a -> Maybe (NonNegative a)
mkNonNegative x
  | x >= 0 = Just (UnsafeNonNegative x)
  | otherwise = Nothing
{-# INLINEABLE mkNonNegative #-}

-- | Variant of 'mkNonNegative' that throws an error when given a value < 0.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> unsafeNonNegative 7
-- UnsafeNonNegative {unNonNegative = 7}
--
-- @since 0.1
unsafeNonNegative :: (HasCallStack, Num a, Ord a, Show a) => a -> NonNegative a
unsafeNonNegative x
  | x >= 0 = UnsafeNonNegative x
  | otherwise =
      error $
        "Numeric.Data.NonNegative.unsafeNonNegative: Passed value < 0: " <> show x
{-# INLINEABLE unsafeNonNegative #-}

-- | This function is an alias for the unchecked constructor @UnsafeNonNegative@
-- i.e. it allows us to construct a 'NonNegative' __without__ checking the
-- invariant. This is intended only for when we absolutely know the invariant
-- holds and a branch (i.e. 'unsafeNonNegative') is undesirable for performance
-- reasons. Exercise extreme caution.
--
-- @since 0.1
reallyUnsafeNonNegative :: a -> NonNegative a
reallyUnsafeNonNegative = UnsafeNonNegative
{-# INLINEABLE reallyUnsafeNonNegative #-}
