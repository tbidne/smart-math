{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides the 'Fraction' type, a safer alternative to 'Ratio'.
--
-- @since 0.1
module Numeric.Data.Fraction.Base.Internal
  ( -- * Type
    Fraction ((:%:), (:%!), UnsafeFraction),

    -- * Creation
    unsafeFraction,
    (%!),

    -- * Elimination
    numerator,
    denominator,

    -- * Functions
    reduce,

    -- * Misc
    errMsg,
  )
where

import Control.DeepSeq (NFData)
import Data.Bounds
  ( LowerBounded (lowerBound),
    LowerBoundless,
    MaybeLowerBounded (maybeLowerBound),
    UpperBoundless,
  )
import Data.Kind (Type)
import Data.Text.Display (Display (displayBuilder))
import GHC.Generics (Generic)
import GHC.Real (Ratio ((:%)))
import GHC.Real qualified as R
import GHC.Records (HasField (getField))
import GHC.Stack (HasCallStack)
import Language.Haskell.TH.Syntax (Lift)
import Numeric.Algebra.Additive.AGroup (AGroup ((.-.)))
import Numeric.Algebra.Additive.AMonoid (AMonoid (zero))
import Numeric.Algebra.Additive.ASemigroup (ASemigroup ((.+.)))
import Numeric.Algebra.Field (Field)
import Numeric.Algebra.MetricSpace (MetricSpace (diffR))
import Numeric.Algebra.Multiplicative.MGroup (MGroup ((.%.)))
import Numeric.Algebra.Multiplicative.MMonoid (MMonoid (one))
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup ((.*.)))
import Numeric.Algebra.Normed (Normed (norm, sgn))
import Numeric.Algebra.Ring (Ring)
import Numeric.Algebra.Semifield (Semifield)
import Numeric.Algebra.Semiring (Semiring)
import Numeric.Class.Division (Division (divide))
import Numeric.Convert.Integer (FromInteger (fromZ))
import Numeric.Convert.Rational (FromRational (fromQ), ToRational (toQ))
import Numeric.Convert.Real (FromReal (fromR), ToReal (toR))
import Optics.Core
  ( A_Getter,
    A_Lens,
    LabelOptic (labelOptic),
    lens,
    to,
  )

-- $setup
-- >>> :set -XTemplateHaskell

-- | Type for representing fractions. Designed to be similar to 'Ratio' with
-- the primary difference that it does __not__ require the following invariants
-- for its instances (e.g. 'Eq') to be sensible:
--
-- 1. @n / d@ is maximally reduced.
--
-- 2. @d > 0@.
--
-- This has a number of consequences.
--
-- 1. Fraction's 'Eq' is based on an equivalence class, in contrast to
--    'Ratio', which compares the numerator and denominator directly:
--
--        * Fractions are reduced first, e.g., @2 :%: 4 === 1 :%: 2@.
--        * Negative denominators are considered:
--        @1 :%: 1 === -1 :%: -1@.
--
-- 2. The denominator is given more consideration:
--
--        * 'abs' operates on the numerator /and/ the denominator.
--        * 'signum' is positive if /both/ are negative.
--
-- 3. @'Show' x@ does __not__ reduce @x@ first. This is to make debugging
-- easier.
--
-- @'Fraction' 'Integer'@ is a 'Numeric.Algebra.Field.Field', and @'Fraction'
-- 'GHC.Natural.Natural'@ is a 'Numeric.Algebra.Semiring.Semiring'.
--
-- ==== __Examples__
--
-- >>> 2 %! 6 == 1 %! 3
-- True
--
-- >>> 1 %! 1 == -1 %! -1
-- True
--
-- >>> 1 %! 7 >= 1 %! -2
-- True
--
-- >>> -1 %! 7 >= 1 %! -2
-- True
--
-- >>> import Data.Text.Display (display)
-- >>> display $ 2 %! 6
-- "1 / 3"
--
-- @since 0.1
type Fraction :: Type -> Type
data Fraction a = UnsafeFraction !a !a
  deriving stock
    ( -- | @since 0.1
      Generic,
      -- | @since 0.1
      Lift,
      -- @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance HasField "numerator" (Fraction n) n where
  getField (UnsafeFraction n _) = n

-- | @since 0.1
instance HasField "denominator" (Fraction n) n where
  getField (UnsafeFraction _ d) = d

-- | @since 0.1
instance
  ( k ~ A_Lens,
    a ~ n,
    b ~ n
  ) =>
  LabelOptic "numerator" k (Fraction n) (Fraction n) a b
  where
  labelOptic = lens numerator (\(UnsafeFraction _ d) n' -> UnsafeFraction n' d)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  ( k ~ A_Getter,
    a ~ n,
    b ~ n
  ) =>
  LabelOptic "denominator" k (Fraction n) (Fraction n) a b
  where
  labelOptic = to denominator
  {-# INLINE labelOptic #-}

-- | Unidirectional pattern synonym for 'Fraction'. This allows us to pattern
-- match on a fraction term without exposing the unsafe internal details.
--
-- @since 0.1
pattern (:%:) :: a -> a -> Fraction a
pattern n :%: d <- UnsafeFraction n d

{-# COMPLETE (:%:) #-}

infixr 5 :%:

-- | Bidirectional pattern synonym for 'Fraction'. Note that this is __not__
-- safe in general, as construction with a zero denominator with throw an
-- error.
--
-- __WARNING: Partial__
--
-- @since 0.1
pattern (:%!) ::
  ( HasCallStack,
    Integral a,
    UpperBoundless a
  ) =>
  a ->
  a ->
  Fraction a
pattern n :%! d <- UnsafeFraction n d
  where
    n :%! d = unsafeFraction n d

{-# COMPLETE (:%!) #-}

infixr 5 :%!

-- NOTE: No UpperBounded (consequently no Bounded) instance because we intend
-- for Fraction to only be used with types w/o an upper bound.

-- | @since 0.1
instance (LowerBounded a, Num a) => LowerBounded (Fraction a) where
  lowerBound = UnsafeFraction lowerBound 1
  {-# INLINEABLE lowerBound #-}

-- | @since 0.1
instance (MaybeLowerBounded a, Num a) => MaybeLowerBounded (Fraction a) where
  maybeLowerBound = (\n -> UnsafeFraction n 1) <$> maybeLowerBound
  {-# INLINEABLE maybeLowerBound #-}

-- | @since 0.1
instance (LowerBoundless a) => LowerBoundless (Fraction a)

-- | @since 0.1
instance (UpperBoundless a) => UpperBoundless (Fraction a)

-- | @since 0.1
instance (Show a) => Display (Fraction a) where
  displayBuilder (UnsafeFraction n d) =
    mconcat
      [ displayBuilder $ show n,
        displayBuilder @String " / ",
        displayBuilder $ show d
      ]

-- | @since 0.1
instance (Eq a, Integral a, UpperBoundless a) => Eq (Fraction a) where
  UnsafeFraction 0 _ == UnsafeFraction 0 _ = True
  x == y = n1 == n2 && d1 == d2
    where
      UnsafeFraction n1 d1 = reduce x
      UnsafeFraction n2 d2 = reduce y
  {-# INLINEABLE (==) #-}

-- | @since 0.1
instance (Integral a, UpperBoundless a) => Ord (Fraction a) where
  x@(UnsafeFraction n1 d1) <= y@(UnsafeFraction n2 d2)
    | x == y = True
    | otherwise = n1 * d2 `comp` n2 * d1
    where
      isNeg = (< 0) . denominator
      comp
        | isNeg x `xor` isNeg y = (>=)
        | otherwise = (<=)
      infix 4 `comp`
  {-# INLINEABLE (<=) #-}

-- | @since 0.1
instance (Integral a, UpperBoundless a) => Enum (Fraction a) where
  toEnum n = UnsafeFraction (fromIntegral n) 1
  {-# INLINEABLE toEnum #-}
  fromEnum = fromInteger . truncate
  {-# INLINEABLE fromEnum #-}

-- | @since 0.1
instance (Integral a, UpperBoundless a) => Fractional (Fraction a) where
  (UnsafeFraction n1 d1) / (UnsafeFraction n2 d2) =
    unsafeFraction (n1 * d2) (n2 * d1)
  {-# INLINEABLE (/) #-}
  recip (UnsafeFraction 0 _) =
    error "Numeric.Data.Fraction.Base.recip: Fraction has zero numerator"
  recip (UnsafeFraction n d) = unsafeFraction d n
  {-# INLINEABLE recip #-}
  fromRational (n :% d) = unsafeFraction (fromInteger n) (fromInteger d)
  {-# INLINEABLE fromRational #-}

-- | @since 0.1
instance (Integral a, UpperBoundless a) => Num (Fraction a) where
  (UnsafeFraction n1 d1) + (UnsafeFraction n2 d2) =
    unsafeFraction (n1 * d2 + n2 * d1) (d1 * d2)
  {-# INLINEABLE (+) #-}
  (UnsafeFraction n1 d1) - (UnsafeFraction n2 d2) =
    unsafeFraction (n1 * d2 - n2 * d1) (d1 * d2)
  {-# INLINEABLE (-) #-}
  (UnsafeFraction n1 d1) * (UnsafeFraction n2 d2) =
    unsafeFraction (n1 * n2) (d1 * d2)
  {-# INLINEABLE (*) #-}
  negate (UnsafeFraction n d) = UnsafeFraction (-n) d
  {-# INLINEABLE negate #-}
  abs (UnsafeFraction n d) = UnsafeFraction (abs n) (abs d)
  {-# INLINEABLE abs #-}
  signum (UnsafeFraction n d) = UnsafeFraction (signum n * signum d) 1
  {-# INLINEABLE signum #-}
  fromInteger n1 = UnsafeFraction (fromInteger n1) 1
  {-# INLINEABLE fromInteger #-}

-- | @since 0.1
instance (Integral a, UpperBoundless a) => Real (Fraction a) where
  toRational (UnsafeFraction n d) = R.reduce (fromIntegral n) (fromIntegral d)
  {-# INLINEABLE toRational #-}

-- | @since 0.1
instance (Integral a, UpperBoundless a) => RealFrac (Fraction a) where
  properFraction (UnsafeFraction n d) =
    (fromInteger (toInteger q), UnsafeFraction r d)
    where
      (q, r) = quotRem n d
  {-# INLINEABLE properFraction #-}

-- | @since 0.1
instance (Integral a, UpperBoundless a) => Division (Fraction a) where
  divide = (/)
  {-# INLINEABLE divide #-}

-- | @since 0.1
instance (Integral a, UpperBoundless a) => ASemigroup (Fraction a) where
  (.+.) = (+)
  {-# INLINEABLE (.+.) #-}

-- | @since 0.1
instance (Integral a, UpperBoundless a) => AMonoid (Fraction a) where
  zero = UnsafeFraction 0 1
  {-# INLINEABLE zero #-}

-- | @since 0.1
instance (Integral a, UpperBoundless a) => AGroup (Fraction a) where
  (.-.) = (-)
  {-# INLINEABLE (.-.) #-}

-- | @since 0.1
instance (Integral a, UpperBoundless a) => MSemigroup (Fraction a) where
  (.*.) = (*)
  {-# INLINEABLE (.*.) #-}

-- | @since 0.1
instance (Integral a, UpperBoundless a) => MMonoid (Fraction a) where
  one = UnsafeFraction 1 1
  {-# INLINEABLE one #-}

-- | @since 0.1
instance (Integral a, UpperBoundless a) => MGroup (Fraction a) where
  x .%. (UnsafeFraction n d) = x .*. UnsafeFraction d n
  {-# INLINEABLE (.%.) #-}

-- | @since 0.1
instance (Integral a, UpperBoundless a) => MetricSpace (Fraction a) where
  diffR x y
    | x <= y = realToFrac $ abs (y - x)
    | otherwise = realToFrac $ abs (x - y)
  {-# INLINEABLE diffR #-}

-- | @since 0.1
instance (Integral a, UpperBoundless a) => Normed (Fraction a) where
  norm = abs
  {-# INLINEABLE norm #-}

  sgn = signum
  {-# INLINEABLE sgn #-}

-- | @since 0.1
instance (Integral a, UpperBoundless a) => Semiring (Fraction a)

-- | @since 0.1
instance (Integral a, UpperBoundless a) => Ring (Fraction a)

-- | @since 0.1
instance (Integral a, UpperBoundless a) => Semifield (Fraction a)

-- | @since 0.1
instance (Integral a, UpperBoundless a) => Field (Fraction a)

-- | @since 0.1
instance (Integral a, UpperBoundless a) => FromInteger (Fraction a) where
  fromZ = fromInteger
  {-# INLINEABLE fromZ #-}

-- | @since 0.1
instance (Integral a, UpperBoundless a) => FromRational (Fraction a) where
  fromQ = fromRational
  {-# INLINEABLE fromQ #-}

-- | @since 0.1
instance (Integral a, UpperBoundless a) => ToRational (Fraction a) where
  toQ (UnsafeFraction n d) = toRational (n :% d)
  {-# INLINEABLE toQ #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance (Integral a, UpperBoundless a) => FromReal (Fraction a) where
  fromR = fromRational . realToFrac
  {-# INLINEABLE fromR #-}

-- | @since 0.1
instance (Integral a, UpperBoundless a) => ToReal (Fraction a) where
  toR (UnsafeFraction n d) = realToFrac (n :% d)
  {-# INLINEABLE toR #-}

-- | @since 0.1
numerator :: Fraction a -> a
numerator (UnsafeFraction n _) = n

-- | @since 0.1
denominator :: Fraction a -> a
denominator (UnsafeFraction _ d) = d

-- | Throws an error when given a denominator of 0.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> unsafeFraction 7 2
-- UnsafeFraction 7 2
--
--
-- @since 0.1
unsafeFraction ::
  ( HasCallStack,
    Integral a,
    UpperBoundless a
  ) =>
  a ->
  a ->
  Fraction a
unsafeFraction _ 0 = error $ errMsg "unsafeFraction"
unsafeFraction n d = reduce $ UnsafeFraction n d
{-# INLINEABLE unsafeFraction #-}

-- | Infix version of 'unsafeFraction'.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
--
-- >>> 7 %! 2
-- UnsafeFraction 7 2
--
-- @since 0.1
(%!) ::
  ( HasCallStack,
    Integral a,
    UpperBoundless a
  ) =>
  a ->
  a ->
  Fraction a
n %! d = unsafeFraction n d
{-# INLINE (%!) #-}

infixl 7 %!

-- | Reduces a fraction:
--
-- 1. Removes common factors.
-- 2. Factors out negative denominators.
-- 3. @reduce (0 :%: _) --> 0 :%: 1@.
--
-- ==== __Examples__
-- >>> reduce (7 %! 2)
-- UnsafeFraction 7 2
--
-- >>> reduce (18 %! 10)
-- UnsafeFraction 9 5
--
-- >>> reduce (-5 %! -5)
-- UnsafeFraction 1 1
--
-- @since 0.1
reduce :: (Integral a, UpperBoundless a) => Fraction a -> Fraction a
reduce (UnsafeFraction 0 _) = UnsafeFraction 0 1
reduce (UnsafeFraction n d) = UnsafeFraction (n' * signum d) (abs d')
  where
    n' = n `quot` g
    d' = d `quot` g
    g = gcd n d
{-# INLINEABLE reduce #-}

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False
{-# INLINEABLE xor #-}

infixr 2 `xor`

-- | @since 0.1
errMsg :: String -> String
errMsg fn =
  mconcat
    [ "Numeric.Data.Fraction.Base.",
      fn,
      ": Fraction has zero denominator"
    ]
