{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides the 'Fraction' type, a safer alternative to 'Ratio'.
--
-- @since 0.1.0.0
module Numeric.Data.Fraction
  ( -- * Type
    Fraction ((:%:)),

    -- * Creation
    mkFraction,
    mkFractionTH,
    unsafeFraction,

    -- * Elimination
    numerator,
    denominator,

    -- * Functions
    reduce,
  )
where

import Control.DeepSeq (NFData)
import Data.Kind (Type)
import Data.Maybe qualified as May
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import GHC.Read (Read (..))
import GHC.Read qualified as Read
import GHC.Real (Ratio (..))
import GHC.Real qualified as R
import GHC.Stack (HasCallStack)
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
import Numeric.Algebra.Semiring (Semiring)
import Numeric.Class.Boundless (UpperBoundless)
import Numeric.Class.Division (Division (..))
import Numeric.Data.NonZero (NonZero (..))
import Text.ParserCombinators.ReadPrec qualified as ReadP
import Text.Read.Lex qualified as L

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
-- easier. Furthermore, @read . show@ is a roundtrip.
--
-- @'Fraction' 'Integer'@ is a 'Numeric.Algebra.Field.Field', and @'Fraction'
-- 'GHC.Natural.Natural'@ is a 'Numeric.Algebra.Semiring.Semiring'.
--
-- ==== __Examples__
--
-- >>> 2 :%: 6 == 1 :%: 3
-- True
--
-- >>> 1 :%: 1 == -1 :%: -1
-- True
--
-- >>> 1 :%: 7 >= 1 :%: -2
-- True
--
-- >>> -1 :%: 7 >= 1 :%: -2
-- True
--
-- >>> read @(Fraction Integer) $ show (123 :%: -3461)
-- (-123) :%: 3461
--
-- @since 0.1.0.0
type Fraction :: Type -> Type
data Fraction a = UnsafeFraction !a !a
  deriving stock
    ( -- | @since 0.1.0.0
      Generic,
      -- | @since 0.1.0.0
      Lift
    )
  deriving anyclass
    ( -- | @since 0.1.0.0
      NFData
    )

-- | Bidirectional pattern synonym for 'Fraction'. The constructor is an alias
-- for 'unsafeFraction'.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> 2 :%: 4
-- 1 :%: 2
--
-- >>> 1 :%: 0
-- *** Exception: Ratio has zero denominator
--
-- @since 0.1.0.0
pattern (:%:) :: (HasCallStack, UpperBoundless a) => a -> a -> Fraction a
pattern n :%: d <-
  UnsafeFraction n d
  where
    n :%: d = unsafeFraction n d

{-# COMPLETE (:%:) #-}

infixr 5 :%:

-- | @since 0.1.0.0
instance (Integral a, Show a) => Show (Fraction a) where
  showsPrec i (UnsafeFraction n d) =
    showParen
      (i >= 11)
      (showsPrec 11 n . showString " :%: " . showsPrec 11 d)

-- | @since 0.1.0.0
instance (Eq a, UpperBoundless a) => Eq (Fraction a) where
  0 :%: _ == 0 :%: _ = True
  x == y = n1 == n2 && d1 == d2
    where
      n1 :%: d1 = reduce x
      n2 :%: d2 = reduce y

-- | @since 0.1.0.0
instance UpperBoundless a => Ord (Fraction a) where
  x@(n1 :%: d1) <= y@(n2 :%: d2)
    | x == y = True
    | otherwise = n1 * d2 `comp` n2 * d1
    where
      isNeg = (< 0) . denominator
      comp
        | isNeg x `xor` isNeg y = (>=)
        | otherwise = (<=)
      infix 4 `comp`

-- | @since 0.1.0.0
instance UpperBoundless a => Enum (Fraction a) where
  toEnum n = UnsafeFraction (fromIntegral n) 1
  fromEnum = fromInteger . truncate

-- | @since 0.1.0.0
instance UpperBoundless a => Fractional (Fraction a) where
  (n1 :%: d1) / (n2 :%: d2) = unsafeFraction (n1 * d2) (n2 * d1)
  recip (0 :%: _) = R.ratioZeroDenominatorError
  recip (n :%: d) = unsafeFraction d n
  fromRational (n :% d) = unsafeFraction (fromInteger n) (fromInteger d)

-- | @since 0.1.0.0
instance UpperBoundless a => Num (Fraction a) where
  (n1 :%: d1) + (n2 :%: d2) = unsafeFraction (n1 * d2 + n2 * d1) (d1 * d2)
  (n1 :%: d1) - (n2 :%: d2) = unsafeFraction (n1 * d2 - n2 * d1) (d1 * d2)
  (n1 :%: d1) * (n2 :%: d2) = unsafeFraction (n1 * n2) (d1 * d2)
  negate (n :%: d) = UnsafeFraction (-n) d
  abs (n :%: d) = UnsafeFraction (abs n) (abs d)
  signum (n :%: d) = UnsafeFraction (signum n * signum d) 1
  fromInteger n1 = UnsafeFraction (fromInteger n1) 1

-- | @since 0.1.0.0
instance UpperBoundless a => Real (Fraction a) where
  toRational (n :%: d) = R.reduce (fromIntegral n) (fromIntegral d)

-- | @since 0.1.0.0
instance UpperBoundless a => RealFrac (Fraction a) where
  properFraction (n :%: d) =
    (fromInteger (toInteger q), UnsafeFraction r d)
    where
      (q, r) = quotRem n d

-- | @since 0.1.0.0
instance (Read a, UpperBoundless a) => Read (Fraction a) where
  readPrec =
    Read.parens
      ( ReadP.prec
          7
          ( do
              x <- ReadP.step readPrec
              Read.expectP (L.Symbol ":%:")
              y <- ReadP.step readPrec
              return (UnsafeFraction x y)
          )
      )

-- | @since 0.1.0.0
instance Division (Fraction Integer) where
  divide = (/)

-- | @since 0.1.0.0
instance Division (Fraction Natural) where
  divide = (/)

-- | @since 0.1.0.0
instance ASemigroup (Fraction Integer) where
  type AddConstraint (Fraction Integer) = Fraction Integer
  (.+.) = (+)

-- | @since 0.1.0.0
instance ASemigroup (Fraction Natural) where
  type AddConstraint (Fraction Natural) = Fraction Natural
  (.+.) = (+)

-- | @since 0.1.0.0
instance AMonoid (Fraction Integer) where
  zero = 0 :%: 1

-- | @since 0.1.0.0
instance AMonoid (Fraction Natural) where
  zero = 0 :%: 1

-- | @since 0.1.0.0
instance AGroup (Fraction Integer) where
  type SubtractConstraint (Fraction Integer) = Fraction Integer
  (.-.) = (-)
  aabs = abs

-- | @since 0.1.0.0
instance MSemigroup (Fraction Integer) where
  type MultConstraint (Fraction Integer) = Fraction Integer
  (.*.) = (*)

-- | @since 0.1.0.0
instance MSemigroup (Fraction Natural) where
  type MultConstraint (Fraction Natural) = Fraction Natural
  (.*.) = (*)

-- | @since 0.1.0.0
instance MMonoid (Fraction Integer) where
  one = 1 :%: 1

-- | @since 0.1.0.0
instance MMonoid (Fraction Natural) where
  one = 1 :%: 1

-- | @since 0.1.0.0
instance MGroup (Fraction Integer) where
  type DivConstraint (Fraction Integer) = NonZero (Fraction Integer)
  x .%. MkNonZero (n :%: d) = x .*. (d :%: n)

-- | @since 0.1.0.0
instance MGroup (Fraction Natural) where
  type DivConstraint (Fraction Natural) = NonZero (Fraction Natural)
  x .%. MkNonZero (n :%: d) = x .*. (d :%: n)

-- | @since 0.1.0.0
instance Semiring (Fraction Integer)

-- | @since 0.1.0.0
instance Semiring (Fraction Natural)

-- | @since 0.1.0.0
instance Ring (Fraction Integer)

-- | @since 0.1.0.0
instance Field (Fraction Integer)

-- | Smart constructor for 'Fraction'. Returns 'Nothing' if the second
-- parameter is 0. Reduces the fraction via 'reduce' if possible.
--
-- ==== __Examples__
-- >>> mkFraction 10 4
-- Just (5 :%: 2)
--
-- >>> mkFraction 10 0
-- Nothing
--
-- @since 0.1.0.0
mkFraction :: UpperBoundless a => a -> a -> Maybe (Fraction a)
mkFraction _ 0 = Nothing
mkFraction n d = Just $ reduce (UnsafeFraction n d)

-- | Template haskell for creating a 'Fraction' at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkFractionTH 7 2)
-- 7 :%: 2
--
-- @since 0.1.0.0
#if MIN_VERSION_template_haskell(2,17,0)
mkFractionTH :: (Lift a, UpperBoundless a) => a -> a -> Code Q (Fraction a)
#else
mkFractionTH :: (Lift a, UpperBoundless a) => a -> a -> Q (TExp (Fraction a))
#endif
mkFractionTH n = maybe R.ratioZeroDenominatorError liftTyped . mkFraction n

-- | Variant of 'mkFraction' that throws 'R.ratioZeroDenominatorError' when
-- given a denominator of 0.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> unsafeFraction 7 2
-- 7 :%: 2
--
-- >>> unsafeFraction 7 0
-- *** Exception: Ratio has zero denominator
--
-- @since 0.1.0.0
unsafeFraction :: (HasCallStack, UpperBoundless a) => a -> a -> Fraction a
unsafeFraction n = May.fromMaybe R.ratioZeroDenominatorError . mkFraction n

-- | Returns the numerator.
--
-- ==== __Examples__
-- >>> numerator (-123 :%: 200)
-- -123
--
-- @since 0.1.0.0
numerator :: UpperBoundless a => Fraction a -> a
numerator (n :%: _) = n

-- | Returns the denominator.
--
-- ==== __Examples__
-- >>> denominator (4 :%: 17)
-- 17
--
-- @since 0.1.0.0
denominator :: UpperBoundless a => Fraction a -> a
denominator (_ :%: d) = d

-- | Reduces a fraction:
--
-- 1. Removes common factors.
-- 2. Factors out negative denominators.
-- 3. @reduce (0 :%: _) --> 0 :%: 1@.
--
-- ==== __Examples__
-- >>> reduce (7 :%: 2)
-- 7 :%: 2
--
-- >>> reduce (18 :%: 10)
-- 9 :%: 5
--
-- >>> reduce (-5 :%: -5)
-- 1 :%: 1
--
-- @since 0.1.0.0
reduce :: UpperBoundless a => Fraction a -> Fraction a
reduce (UnsafeFraction 0 _) = UnsafeFraction 0 1
reduce (UnsafeFraction n d) = UnsafeFraction (n' * signum d) (abs d')
  where
    n' = n `quot` g
    d' = d `quot` g
    g = gcd n d

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

infixr 2 `xor`
