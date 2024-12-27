{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'NonZero' type for enforcing a non-zero invariant.
--
-- @since 0.1
module Numeric.Data.NonZero.Algebra.Internal
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
import Numeric.Algebra (AMonoid)
import Numeric.Algebra.Additive (pattern NonZero, pattern Zero)
import Numeric.Algebra.MetricSpace (MetricSpace (diffR))
import Numeric.Algebra.Multiplicative
  ( MEuclidean (mdivMod),
    MGroup ((.%.)),
    MMonoid (one),
    MSemigroup ((.*.)),
  )
import Numeric.Algebra.Normed (Normed (norm, sgn))
import Numeric.Convert.Integer (FromInteger (fromZ), ToInteger (toZ))
import Numeric.Convert.Rational (FromRational (fromQ), ToRational (toQ))
import Numeric.Convert.Real (FromReal (fromR), ToReal (toR))
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
instance (MSemigroup a) => MSemigroup (NonZero a) where
  (.*.) = liftNonZero2 (.*.)
  {-# INLINE (.*.) #-}

-- | @since 0.1
instance (MMonoid a) => MMonoid (NonZero a) where
  one = UnsafeNonZero one
  {-# INLINE one #-}

-- | @since 0.1
instance (MGroup a) => MGroup (NonZero a) where
  (.%.) = liftNonZero2 (.%.)
  {-# INLINE (.%.) #-}

-- | @since 0.1
instance (MEuclidean a) => MEuclidean (NonZero a) where
  UnsafeNonZero x `mdivMod` UnsafeNonZero d =
    bimap UnsafeNonZero UnsafeNonZero $ x `mdivMod` d
  {-# INLINE mdivMod #-}

-- | @since 0.1
instance (MetricSpace a) => MetricSpace (NonZero a) where
  diffR = applyNonZero2 diffR
  {-# INLINEABLE diffR #-}

-- | @since 0.1
instance (Normed a) => Normed (NonZero a) where
  norm (UnsafeNonZero x) = UnsafeNonZero $ norm x
  {-# INLINEABLE norm #-}

  sgn (UnsafeNonZero x) = UnsafeNonZero $ sgn x
  {-# INLINEABLE sgn #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance (AMonoid a, Eq a, FromInteger a) => FromInteger (NonZero a) where
  fromZ = unsafeNonZero . fromZ
  {-# INLINE fromZ #-}

-- | @since 0.1
instance (ToInteger a) => ToInteger (NonZero a) where
  toZ (UnsafeNonZero x) = toZ x
  {-# INLINEABLE toZ #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance (AMonoid a, Eq a, FromRational a) => FromRational (NonZero a) where
  fromQ = unsafeNonZero . fromQ
  {-# INLINE fromQ #-}

-- | @since 0.1
instance (ToRational a) => ToRational (NonZero a) where
  toQ (UnsafeNonZero x) = toQ x
  {-# INLINEABLE toQ #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance (AMonoid a, Eq a, FromReal a) => FromReal (NonZero a) where
  fromR = unsafeNonZero . fromR
  {-# INLINEABLE fromR #-}

-- | @since 0.1
instance (ToReal a) => ToReal (NonZero a) where
  toR (UnsafeNonZero x) = toR x
  {-# INLINEABLE toR #-}

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
unsafeNonZero :: (AMonoid a, Eq a, HasCallStack) => a -> NonZero a
unsafeNonZero Zero = error $ errMsg "unsafeNonZero"
unsafeNonZero (NonZero x) = UnsafeNonZero x
{-# INLINEABLE unsafeNonZero #-}

-- | @since 0.1
errMsg :: String -> String
errMsg fn =
  mconcat
    [ "Numeric.Data.NonZero.Algebra.",
      fn,
      ": Received zero"
    ]

liftNonZero2 ::
  forall a.
  (a -> a -> a) ->
  NonZero a ->
  NonZero a ->
  NonZero a
liftNonZero2 f x = UnsafeNonZero . applyNonZero2 f x

applyNonZero2 ::
  (a -> a -> r) ->
  NonZero a ->
  NonZero a ->
  r
applyNonZero2 f (UnsafeNonZero x) (UnsafeNonZero y) = f x y
