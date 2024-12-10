{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'Positive' type for enforcing a positive invariant.
--
-- @since 0.1
module Numeric.Data.Positive.Base.Internal
  ( -- * Type
    Positive (MkPositive, UnsafePositive),

    -- * Creation
    unsafePositive,

    -- * Misc
    errMsg,
  )
where

import Control.DeepSeq (NFData)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Bounds (UpperBounded (upperBound), UpperBoundless)
import Data.Kind (Type)
import Data.Text.Display (Display, ShowInstance (ShowInstance))
import GHC.Generics (Generic)
import GHC.Records (HasField (getField))
import GHC.Stack (HasCallStack)
import Language.Haskell.TH.Syntax (Lift)
import Numeric.Algebra.Additive.ASemigroup (ASemigroup ((.+.)))
import Numeric.Algebra.MetricSpace (MetricSpace (diffR))
import Numeric.Algebra.Multiplicative.MEuclidean (MEuclidean (mdivMod))
import Numeric.Algebra.Multiplicative.MGroup (MGroup ((.%.)))
import Numeric.Algebra.Multiplicative.MMonoid (MMonoid (one))
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup ((.*.)))
import Numeric.Algebra.Normed (Normed (norm))
import Numeric.Class.Division (Division (divide))
import Numeric.Convert.Integer (FromInteger (fromZ), ToInteger (toZ))
import Numeric.Convert.Rational (FromRational (fromQ), ToRational (toQ))
import Numeric.Convert.Real (FromReal (fromR), ToReal (toR))
import Numeric.Data.Internal.Utils qualified as Utils
import Optics.Core (A_Getter, LabelOptic (labelOptic), to)

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
  deriving
    ( -- | @since 0.1
      Display
    )
    via (ShowInstance a)

-- | @since 0.1
instance HasField "unPositive" (Positive a) a where
  getField (UnsafePositive x) = x

-- | @since 0.1
instance
  ( k ~ A_Getter,
    x ~ a,
    y ~ a
  ) =>
  LabelOptic "unPositive" k (Positive a) (Positive a) x y
  where
  labelOptic = to (\(UnsafePositive x) -> x)
  {-# INLINE labelOptic #-}

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
instance (Num a) => ASemigroup (Positive a) where
  (.+.) = liftPositive2 (+)
  {-# INLINEABLE (.+.) #-}

-- | @since 0.1
instance (Num a) => MSemigroup (Positive a) where
  (.*.) = liftPositive2 (*)
  {-# INLINEABLE (.*.) #-}

-- | @since 0.1
instance (Num a) => MMonoid (Positive a) where
  one = UnsafePositive 1
  {-# INLINEABLE one #-}

-- | @since 0.1
instance (Division a, Num a) => MGroup (Positive a) where
  (.%.) = liftPositive2 divide
  {-# INLINEABLE (.%.) #-}

-- | @since 0.1
instance (Division a, Integral a) => MEuclidean (Positive a) where
  UnsafePositive x `mdivMod` (UnsafePositive d) =
    bimap UnsafePositive UnsafePositive $ x `divMod` d
  {-# INLINEABLE mdivMod #-}

-- | @since 0.1
instance (Real a) => MetricSpace (Positive a) where
  diffR = applyPositive2 Utils.safeDiff
  {-# INLINEABLE diffR #-}

-- | @since 0.1
instance Normed (Positive a) where
  norm = id
  {-# INLINEABLE norm #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance (Num a, Ord a, Show a) => FromInteger (Positive a) where
  fromZ = unsafePositive . fromInteger
  {-# INLINEABLE fromZ #-}

-- | @since 0.1
instance (Integral a) => ToInteger (Positive a) where
  toZ (UnsafePositive x) = toInteger x
  {-# INLINEABLE toZ #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance (Fractional a, Ord a, Show a) => FromRational (Positive a) where
  fromQ = unsafePositive . fromRational
  {-# INLINEABLE fromQ #-}

-- | @since 0.1
instance (Real a) => ToRational (Positive a) where
  toQ (UnsafePositive x) = toRational x
  {-# INLINEABLE toQ #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance (Fractional a, Ord a, Show a) => FromReal (Positive a) where
  fromR = unsafePositive . realToFrac
  {-# INLINEABLE fromR #-}

-- | @since 0.1
instance (Real a) => ToReal (Positive a) where
  toR (UnsafePositive x) = realToFrac x
  {-# INLINEABLE toR #-}

-- | Throws an error when given a value <= 0.
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
        errMsg "unsafePositive" x
{-# INLINEABLE unsafePositive #-}

-- | @since 0.1
errMsg :: (Show a) => String -> a -> String
errMsg fn x =
  mconcat
    [ "Numeric.Data.Positive.Base.",
      fn,
      ": Received value <= zero: ",
      show x
    ]

liftPositive2 ::
  forall a.
  (a -> a -> a) ->
  Positive a ->
  Positive a ->
  Positive a
liftPositive2 f x = UnsafePositive . applyPositive2 f x

applyPositive2 ::
  (a -> a -> r) ->
  Positive a ->
  Positive a ->
  r
applyPositive2 f (UnsafePositive x) (UnsafePositive y) = f x y
