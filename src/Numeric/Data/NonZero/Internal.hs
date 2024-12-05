{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'NonZero' type for enforcing a non-zero invariant.
--
-- @since 0.1
module Numeric.Data.NonZero.Internal
  ( -- * Type
    NonZero (MkNonZero, UnsafeNonZero),

    -- * Creation
    unsafeNonZero,

    -- * Misc
    errMsg,
  )
where

import Control.DeepSeq (NFData)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Kind (Type)
import Data.Text.Display (Display, ShowInstance (ShowInstance))
import GHC.Generics (Generic)
import GHC.Records (HasField (getField))
import GHC.Stack (HasCallStack)
import Language.Haskell.TH.Syntax (Lift)
import Numeric.Algebra.MetricSpace (MetricSpace (diff))
import Numeric.Algebra.Multiplicative
  ( MEuclidean (mdivMod),
    MGroup ((.%.)),
    MMonoid (one),
    MSemigroup ((.*.)),
  )
import Numeric.Algebra.Normed (Normed (norm))
import Numeric.Class.Division (Division (divide))
import Numeric.Data.Internal.Utils qualified as Utils
import Numeric.Literal.Integer (FromInteger (afromInteger))
import Numeric.Literal.Rational (FromRational (afromRational))
import Optics.Core (A_Getter, LabelOptic (labelOptic), to)

-- $setup
-- >>> :set -XTemplateHaskell

-- | Smart-constructor for creating a \"non-zero\" @a@.
--
-- @since 0.1
type NonZero :: Type -> Type
newtype NonZero a = UnsafeNonZero a
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
  deriving
    ( -- | @since 0.1
      Display
    )
    via (ShowInstance a)

-- | @since 0.1
instance HasField "unNonZero" (NonZero a) a where
  getField (UnsafeNonZero x) = x

-- | @since 0.1
instance
  ( k ~ A_Getter,
    x ~ a,
    y ~ a
  ) =>
  LabelOptic "unNonZero" k (NonZero a) (NonZero a) x y
  where
  labelOptic = to (\(UnsafeNonZero x) -> x)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance (Num a) => MSemigroup (NonZero a) where
  UnsafeNonZero x .*. UnsafeNonZero y = UnsafeNonZero $ x * y
  {-# INLINE (.*.) #-}

-- | @since 0.1
instance (Num a) => MMonoid (NonZero a) where
  one = UnsafeNonZero 1
  {-# INLINE one #-}

-- | @since 0.1
instance (Division a, Num a) => MGroup (NonZero a) where
  UnsafeNonZero x .%. UnsafeNonZero d = UnsafeNonZero (x `divide` d)
  {-# INLINE (.%.) #-}

-- | @since 0.1
instance (Division a, Integral a) => MEuclidean (NonZero a) where
  UnsafeNonZero x `mdivMod` UnsafeNonZero d =
    bimap UnsafeNonZero UnsafeNonZero $ x `divMod` d
  {-# INLINE mdivMod #-}

-- | @since 0.1
instance (Real a) => MetricSpace (NonZero a) where
  diff (UnsafeNonZero x) (UnsafeNonZero y) = Utils.safeDiff x y
  {-# INLINEABLE diff #-}

-- | @since 0.1
instance (Num a) => Normed (NonZero a) where
  norm (UnsafeNonZero x) = UnsafeNonZero $ abs x
  {-# INLINEABLE norm #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance (FromInteger a, Num a, Ord a) => FromInteger (NonZero a) where
  afromInteger = unsafeNonZero . afromInteger
  {-# INLINE afromInteger #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance (FromRational a, Num a, Ord a) => FromRational (NonZero a) where
  afromRational = unsafeNonZero . afromRational
  {-# INLINE afromRational #-}

-- | Unidirectional pattern synonym for 'NonZero'. This allows us to pattern
-- match on a nonzero term without exposing the unsafe internal details.
--
-- @since 0.1
pattern MkNonZero :: a -> NonZero a
pattern MkNonZero x <- UnsafeNonZero x

{-# COMPLETE MkNonZero #-}

-- | Throws an error when given 0.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> unsafeNonZero 7
-- UnsafeNonZero 7
--
-- @since 0.1
unsafeNonZero :: (Eq a, HasCallStack, Num a) => a -> NonZero a
unsafeNonZero x
  | x == 0 = error $ errMsg "unsafeNonZero"
  | otherwise = UnsafeNonZero x
{-# INLINEABLE unsafeNonZero #-}

-- | @since 0.1
errMsg :: String -> String
errMsg fn =
  mconcat
    [ "Numeric.Data.NonZero.",
      fn,
      ": Received zero"
    ]
