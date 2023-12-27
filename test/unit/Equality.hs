module Equality
  ( EqTy (..),
    Equality (..),
    unEquality,
    Epsilon (..),
  )
where

import Data.Kind (Type)
import GHC.Real (Ratio ((:%)))
import GHC.Real qualified as Real

-- | Epsilon used for comparing floating points.
-- Its monoid instance defaults to 1, and semigroup takes the largest
-- non-one.
--
-- @since 0.1
type Epsilon :: Type -> Type
newtype Epsilon a = MkEpsilon {unEpsilon :: a}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Fractional, Num)

instance (Fractional a, Ord a) => Semigroup (Epsilon a) where
  MkEpsilon 1.0 <> y = y
  x <> MkEpsilon 1.0 = x
  MkEpsilon x <> MkEpsilon y =
    if x >= y
      then MkEpsilon x
      else MkEpsilon y

instance (Fractional a, Ord a) => Monoid (Epsilon a) where
  mempty = 1.0

-- | Types of equality.
--
-- @since 0.1
type EqTy :: Type
data EqTy
  = EqExact
  | EqRatio
  | EqEpsilon
  deriving stock (Show)

-- | 'Equality' is intended for when we need to associate types to different
-- notions of equality, and wrapping these types manually in a newtype is not
-- convenient.
--
-- Suppose we want to verify predicate @p@ for some @x@, but -- unfortunately
-- for us -- @x@'s 'Eq' instances is too strong. We could write a newtype @y@
-- for @x@ and define our test in terms of @y@, i.e., test @p(y)@. This might
-- be inconvenient if we need many resources that are written in terms of @x@,
-- or if any of these resources are sufficiently complicated (e.g. polymorphic
-- generators involving type families).
--
-- This is where 'Equality' comes in. We write our entire test nearly in terms
-- of @x@, then at the last moment, verify our properties with 'Equality'.
--
-- For example, if we want to compare two floating points for approximate
-- equality, we can check @MkEqEpsilon 1.0 x == MkEqEpsilon 1.0 y@.
--
-- @since 0.1
type Equality :: EqTy -> Type -> Type
data Equality eq a where
  -- | Exact quality.
  MkEqExact :: (Eq a) => a -> Equality 'EqExact a
  -- | Equality for ratios.
  MkEqRatio :: (Eq a, Integral a) => Ratio a -> Equality 'EqRatio (Ratio a)
  -- | Equality for floating points. We resolve 'Epsilon' differences via
  -- its semigroup instance.
  MkEqEpsilon :: (RealFloat a) => Epsilon a -> a -> Equality 'EqEpsilon a

deriving stock instance (Show a) => Show (Equality eq a)

unEquality :: Equality eq a -> a
unEquality (MkEqExact x) = x
unEquality (MkEqRatio x) = x
unEquality (MkEqEpsilon _ x) = x

instance Eq (Equality eq a) where
  MkEqExact x == MkEqExact y = x == y
  MkEqRatio x == MkEqRatio y = eqRatio x y
  MkEqEpsilon e1 x == MkEqEpsilon e2 y = eqEpsilon (e1 <> e2) x y

instance (Ord a) => Ord (Equality eq a) where
  x <= y =
    x == y || x' < y'
    where
      x' = unEquality x
      y' = unEquality y

eqRatio :: (Integral a) => Ratio a -> Ratio a -> Bool
eqRatio (0 :% _) (0 :% _) = True
eqRatio x y = reduce' x == reduce' y
  where
    reduce' (n :% d) = Real.reduce n d

eqEpsilon :: (RealFloat a) => Epsilon a -> a -> a -> Bool
eqEpsilon (MkEpsilon e) x y
  | isNaN x = isNaN y
  | isInfinite x = isInfinite y
  | abs (x - y) < e = True
  | otherwise = False
