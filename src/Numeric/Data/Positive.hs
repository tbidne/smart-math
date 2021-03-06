{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'Positive' type for enforcing a positive invariant.
--
-- @since 0.1
module Numeric.Data.Positive
  ( -- * Type
    Positive (MkPositive),

    -- * Creation
    mkPositiveTH,
    mkPositive,
    unsafePositive,
    reallyUnsafePositive,

    -- * Elimination
    unPositive,

    -- * Functions
    positiveToNonZero,

    -- * Optics
    _MkPositive,
    rmatching,
  )
where

import Control.DeepSeq (NFData)
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
import Numeric.Algebra.Additive.ASemigroup (ASemigroup (..))
import Numeric.Algebra.Multiplicative.MGroup (MGroup (..))
import Numeric.Algebra.Multiplicative.MMonoid (MMonoid (..))
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup (..))
import Numeric.Algebra.Normed (Normed (..))
import Numeric.Class.Division (Division (..))
import Numeric.Class.Literal (NumLiteral (..))
import Numeric.Data.NonZero (NonZero (..), reallyUnsafeNonZero, rmatching)
import Optics.Core
  ( A_Getter,
    LabelOptic (..),
    ReversedPrism',
    ReversibleOptic (re),
    prism,
    to,
  )
#if MIN_VERSION_prettyprinter(1, 7, 1)
import Prettyprinter (Pretty (..))
#endif

-- $setup
-- >>> :set -XTemplateHaskell

-- | Newtype wrapper that attaches a 'Positive' invariant to some @a@.
-- 'Positive' is an 'Numeric.Algebra.Additive.ASemigroup.ASemigroup' and
-- 'Numeric.Algebra.Multiplicative.MGroup.MGroup' i.e. supports addition,
-- multiplication, and division.
--
-- @since 0.1
type Positive :: Type -> Type
newtype Positive a = UnsafePositive
  { -- | @since 0.1
    unPositive :: a
  }
  deriving stock
    ( -- | @since 0.1
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

-- | Unidirectional pattern synonym for 'Positive'. This allows us to pattern
-- match on a positive term without exposing the unsafe internal details.
--
-- @since 0.1
pattern MkPositive :: a -> Positive a
pattern MkPositive x <- UnsafePositive x

{-# COMPLETE MkPositive #-}

-- | @since 0.1
instance (k ~ A_Getter, a ~ n) => LabelOptic "unPositive" k (Positive n) (Positive n) a a where
  labelOptic = to unPositive
  {-# INLINEABLE labelOptic #-}

-- | @since 0.1
instance Pretty a => Pretty (Positive a) where
  pretty (UnsafePositive x) = pretty x
  {-# INLINEABLE pretty #-}

-- | @since 0.1
instance Num a => ASemigroup (Positive a) where
  UnsafePositive x .+. UnsafePositive y = UnsafePositive $ x + y
  {-# INLINEABLE (.+.) #-}

-- | @since 0.1
instance Num a => MSemigroup (Positive a) where
  UnsafePositive x .*. UnsafePositive y = UnsafePositive $ x * y
  {-# INLINEABLE (.*.) #-}

-- | @since 0.1
instance Num a => MMonoid (Positive a) where
  one = UnsafePositive 1
  {-# INLINEABLE one #-}

-- | @since 0.1
instance (Division a, Num a) => MGroup (Positive a) where
  UnsafePositive x .%. MkNonZero (UnsafePositive d) = UnsafePositive $ x `divide` d
  {-# INLINEABLE (.%.) #-}

-- | @since 0.1
instance Normed (Positive a) where
  norm = id
  {-# INLINEABLE norm #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance (Num a, Ord a, Show a) => NumLiteral (Positive a) where
  fromLit = unsafePositive . fromInteger
  {-# INLINEABLE fromLit #-}

-- | Template haskell for creating a 'Positive' at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkPositiveTH 1)
-- UnsafePositive {unPositive = 1}
--
-- @since 0.1
#if MIN_VERSION_template_haskell(2,17,0)
mkPositiveTH :: (Integral a, Lift a, Show a) => a -> Code Q (Positive a)
#else
mkPositiveTH :: (Integral a, Lift a, Show a) => a -> Q (TExp (Positive a))
#endif
mkPositiveTH x = maybe (error err) liftTyped $ mkPositive x
  where
    err =
      "Numeric.Data.Positive.mkPositiveTH: Passed value <= 0: " <> show x
{-# INLINEABLE mkPositiveTH #-}

-- | Smart constructor for 'Positive'. Returns 'Nothing' if the second
-- parameter is @<= 0@.
--
-- ==== __Examples__
-- >>> mkPositive 7
-- Just (UnsafePositive {unPositive = 7})
--
-- >>> mkPositive 0
-- Nothing
--
-- @since 0.1
mkPositive :: (Num a, Ord a) => a -> Maybe (Positive a)
mkPositive x
  | x > 0 = Just (UnsafePositive x)
  | otherwise = Nothing
{-# INLINEABLE mkPositive #-}

-- | Variant of 'mkPositive' that throws an error when given a value <= 0.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> unsafePositive 7
-- UnsafePositive {unPositive = 7}
--
-- @since 0.1
unsafePositive :: (HasCallStack, Num a, Ord a, Show a) => a -> Positive a
unsafePositive x
  | x > 0 = UnsafePositive x
  | otherwise =
      error $
        "Numeric.Data.Positive.unsafePositive: Passed value <= 0: " <> show x
{-# INLINEABLE unsafePositive #-}

-- | This function is an alias for the unchecked constructor @UnsafePositive@
-- i.e. it allows us to construct a 'Positive' __without__ checking the
-- invariant. This is intended only for when we absolutely know the invariant
-- holds and a branch (i.e. 'unsafePositive') is undesirable for performance
-- reasons. Exercise extreme caution.
--
-- @since 0.1
reallyUnsafePositive :: a -> Positive a
reallyUnsafePositive = UnsafePositive
{-# INLINEABLE reallyUnsafePositive #-}

-- | Convenience function for adding a 'NonZero' proof to our 'Positive'.
--
-- ==== __Examples__
-- >>> positiveToNonZero $ unsafePositive 3
-- UnsafeNonZero {unNonZero = UnsafePositive {unPositive = 3}}
--
-- @since 0.1
positiveToNonZero :: Positive a -> NonZero (Positive a)
positiveToNonZero = reallyUnsafeNonZero
{-# INLINEABLE positiveToNonZero #-}

-- | 'ReversedPrism'' that enables total elimination and partial construction.
--
-- ==== __Examples__
--
-- >>> import Optics.Core ((^.))
-- >>> pos = $$(mkPositiveTH 2)
-- >>> pos ^. _MkPositive
-- 2
--
-- >>> rmatching _MkPositive 3
-- Right (UnsafePositive {unPositive = 3})
--
-- >>> rmatching _MkPositive 0
-- Left 0
--
-- @since 0.1
_MkPositive :: (Num a, Ord a) => ReversedPrism' (Positive a) a
_MkPositive = re (prism unPositive g)
  where
    g x = case mkPositive x of
      Nothing -> Left x
      Just x' -> Right x'
{-# INLINEABLE _MkPositive #-}
