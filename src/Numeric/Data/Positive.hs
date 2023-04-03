{-# LANGUAGE CPP #-}
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
    (+!),
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
import Data.Bifunctor (Bifunctor (..))
import Data.Bounds (UpperBounded (upperBound), UpperBoundless)
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
import Numeric.Algebra.Multiplicative.MEuclidean (MEuclidean (..))
import Numeric.Algebra.Multiplicative.MGroup (MGroup (..))
import Numeric.Algebra.Multiplicative.MMonoid (MMonoid (..))
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup (..))
import Numeric.Algebra.Normed (Normed (..))
import Numeric.Class.Division (Division (..))
import Numeric.Data.NonZero (NonZero (..), reallyUnsafeNonZero, rmatching)
import Numeric.Literal.Integer (FromInteger (..))
import Numeric.Literal.Rational (FromRational (..))
import Optics.Core
  ( ReversedPrism',
    ReversibleOptic (re),
    prism,
  )
#if MIN_VERSION_prettyprinter(1, 7, 1)
import Prettyprinter (Pretty (..))
#endif

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> :set -XPostfixOperators

-- | Newtype wrapper that attaches a 'Positive' invariant to some @a@.
-- 'Positive' is an 'Numeric.Algebra.Additive.ASemigroup.ASemigroup' and
-- 'Numeric.Algebra.Multiplicative.MGroup.MGroup' i.e. supports addition,
-- multiplication, and division.
--
-- @since 0.1
type Positive :: Type -> Type
newtype Positive a = UnsafePositive a
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
      NFData,
      -- | @since 0.1
      UpperBoundless
    )

-- | Unidirectional pattern synonym for 'Positive'. This allows us to pattern
-- match on a positive term without exposing the unsafe internal details.
--
-- @since 0.1
pattern MkPositive :: a -> Positive a
pattern MkPositive x <- UnsafePositive x

{-# COMPLETE MkPositive #-}

-- | @since 0.1
instance (Bounded a) => UpperBounded (Positive a) where
  upperBound = UnsafePositive maxBound
  {-# INLINEABLE upperBound #-}

-- | @since 0.1
instance (Pretty a) => Pretty (Positive a) where
  pretty (UnsafePositive x) = pretty x
  {-# INLINEABLE pretty #-}

-- | @since 0.1
instance (Num a) => ASemigroup (Positive a) where
  UnsafePositive x .+. UnsafePositive y = UnsafePositive $ x + y
  {-# INLINEABLE (.+.) #-}

-- | @since 0.1
instance (Num a) => MSemigroup (Positive a) where
  UnsafePositive x .*. UnsafePositive y = UnsafePositive $ x * y
  {-# INLINEABLE (.*.) #-}

-- | @since 0.1
instance (Num a) => MMonoid (Positive a) where
  one = UnsafePositive 1
  {-# INLINEABLE one #-}

-- | @since 0.1
instance (Division a, Num a) => MGroup (Positive a) where
  UnsafePositive x .%. (UnsafePositive d) = UnsafePositive $ x `divide` d
  {-# INLINEABLE (.%.) #-}

-- | @since 0.1
instance (Division a, Integral a) => MEuclidean (Positive a) where
  UnsafePositive x `mdivMod` (UnsafePositive d) =
    bimap UnsafePositive UnsafePositive $ x `divMod` d
  {-# INLINEABLE mdivMod #-}

-- | @since 0.1
instance Normed (Positive a) where
  norm = id
  {-# INLINEABLE norm #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance (Num a, Ord a, Show a) => FromInteger (Positive a) where
  afromInteger = unsafePositive . fromInteger
  {-# INLINEABLE afromInteger #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance (Fractional a, Ord a, Show a) => FromRational (Positive a) where
  afromRational = unsafePositive . fromRational
  {-# INLINEABLE afromRational #-}

-- | Template haskell for creating a 'Positive' at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkPositiveTH 1)
-- UnsafePositive 1
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
-- Just (UnsafePositive 7)
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
-- UnsafePositive 7
--
-- @since 0.1
unsafePositive :: (HasCallStack, Num a, Ord a, Show a) => a -> Positive a
unsafePositive x
  | x > 0 = UnsafePositive x
  | otherwise =
      error $
        "Numeric.Data.Positive.unsafePositive: Passed value <= 0: " <> show x
{-# INLINEABLE unsafePositive #-}

-- | Postfix operator for 'unsafePositive'.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
--
-- >>> (7 +!)
-- UnsafePositive 7
--
-- @since 0.1
(+!) :: (HasCallStack, Num a, Ord a, Show a) => a -> Positive a
(+!) = unsafePositive
{-# INLINE (+!) #-}

infixl 7 +!

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
-- UnsafeNonZero (UnsafePositive 3)
--
-- @since 0.1
positiveToNonZero :: Positive a -> NonZero (Positive a)
positiveToNonZero = reallyUnsafeNonZero
{-# INLINEABLE positiveToNonZero #-}

-- | @since 0.1
unPositive :: Positive a -> a
unPositive (UnsafePositive x) = x
{-# INLINE unPositive #-}

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
-- Right (UnsafePositive 3)
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
