{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides the 'Fraction' type, a safer alternative to 'Ratio'.
--
-- @since 0.1
module Numeric.Data.Fraction
  ( -- * Type
    Fraction ((:%:)),

    -- * Creation
    mkFraction,
    mkFractionTH,
    (%%),
    unsafeFraction,
    (%!),

    -- * Elimination
    numerator,
    denominator,

    -- * Functions
    reduce,

    -- * Optics
    -- $optics
    _MkFraction,
    rmatching,
  )
where

import Control.DeepSeq (NFData)
import Data.Kind (Type)
import Data.Maybe qualified as May
#if !MIN_VERSION_prettyprinter(1, 7, 1)
import Data.Text.Prettyprint.Doc (Pretty (..), (<+>))
#endif
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
import Data.Bounds (UpperBoundless)
import Language.Haskell.TH.Syntax (Lift (..))
import Numeric.Algebra.Additive.AGroup (AGroup (..))
import Numeric.Algebra.Additive.AMonoid (AMonoid (..))
import Numeric.Algebra.Additive.ASemigroup (ASemigroup (..))
import Numeric.Algebra.Field (Field)
import Numeric.Algebra.Multiplicative.MGroup (MGroup (..))
import Numeric.Algebra.Multiplicative.MMonoid (MMonoid (..))
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup (..))
import Numeric.Algebra.Normed (Normed (..))
import Numeric.Algebra.Ring (Ring)
import Numeric.Algebra.Semifield (Semifield)
import Numeric.Algebra.Semiring (Semiring)
import Numeric.Class.Division (Division (..))
import Numeric.Data.NonZero (NonZero (..), rmatching)
import Numeric.Literal.Integer (FromInteger (..))
import Numeric.Literal.Rational (FromRational (..))
import Optics.Core
  ( A_Getter,
    A_Lens,
    LabelOptic (..),
    ReversedPrism',
    ReversibleOptic (re),
    lens,
    prism,
    to,
  )
#if MIN_VERSION_prettyprinter(1, 7, 1)
import Prettyprinter (Pretty (..), (<+>))
#endif
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
-- >>> read @(Fraction Integer) $ show (123 %! -3461)
-- (-123) :%: 3461
--
-- @since 0.1
type Fraction :: Type -> Type
data Fraction a = UnsafeFraction
  { -- | @since 0.1
    numerator :: !a,
    -- | @since 0.1
    denominator :: !a
  }
  deriving stock
    ( -- | @since 0.1
      Generic,
      -- | @since 0.1
      Lift
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance (k ~ A_Lens, a ~ n, b ~ n) => LabelOptic "numerator" k (Fraction n) (Fraction n) a b where
  labelOptic = lens numerator (\(UnsafeFraction _ d) n' -> UnsafeFraction n' d)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance (k ~ A_Getter, a ~ n, b ~ n) => LabelOptic "denominator" k (Fraction n) (Fraction n) a b where
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

-- | @since 0.1
instance (Integral a, Show a) => Show (Fraction a) where
  showsPrec i (UnsafeFraction n d) =
    showParen
      (i >= 11)
      (showsPrec 11 n . showString " :%: " . showsPrec 11 d)
  {-# INLINEABLE showsPrec #-}

-- | @since 0.1
instance Pretty a => Pretty (Fraction a) where
  pretty (UnsafeFraction n d) = pretty n <+> pretty @String ":%:" <+> pretty d
  {-# INLINEABLE pretty #-}

-- | @since 0.1
instance (Eq a, Integral a, UpperBoundless a) => Eq (Fraction a) where
  0 :%: _ == 0 :%: _ = True
  x == y = n1 == n2 && d1 == d2
    where
      n1 :%: d1 = reduce x
      n2 :%: d2 = reduce y
  {-# INLINEABLE (==) #-}

-- | @since 0.1
instance (Integral a, UpperBoundless a) => Ord (Fraction a) where
  x@(n1 :%: d1) <= y@(n2 :%: d2)
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
  (n1 :%: d1) / (n2 :%: d2) = unsafeFraction (n1 * d2) (n2 * d1)
  {-# INLINEABLE (/) #-}
  recip (0 :%: _) = R.ratioZeroDenominatorError
  recip (n :%: d) = unsafeFraction d n
  {-# INLINEABLE recip #-}
  fromRational (n :% d) = unsafeFraction (fromInteger n) (fromInteger d)
  {-# INLINEABLE fromRational #-}

-- | @since 0.1
instance (Integral a, UpperBoundless a) => Num (Fraction a) where
  (n1 :%: d1) + (n2 :%: d2) = unsafeFraction (n1 * d2 + n2 * d1) (d1 * d2)
  {-# INLINEABLE (+) #-}
  (n1 :%: d1) - (n2 :%: d2) = unsafeFraction (n1 * d2 - n2 * d1) (d1 * d2)
  {-# INLINEABLE (-) #-}
  (n1 :%: d1) * (n2 :%: d2) = unsafeFraction (n1 * n2) (d1 * d2)
  {-# INLINEABLE (*) #-}
  negate (n :%: d) = UnsafeFraction (-n) d
  {-# INLINEABLE negate #-}
  abs (n :%: d) = UnsafeFraction (abs n) (abs d)
  {-# INLINEABLE abs #-}
  signum (n :%: d) = UnsafeFraction (signum n * signum d) 1
  {-# INLINEABLE signum #-}
  fromInteger n1 = UnsafeFraction (fromInteger n1) 1
  {-# INLINEABLE fromInteger #-}

-- | @since 0.1
instance (Integral a, UpperBoundless a) => Real (Fraction a) where
  toRational (n :%: d) = R.reduce (fromIntegral n) (fromIntegral d)
  {-# INLINEABLE toRational #-}

-- | @since 0.1
instance (Integral a, UpperBoundless a) => RealFrac (Fraction a) where
  properFraction (n :%: d) =
    (fromInteger (toInteger q), UnsafeFraction r d)
    where
      (q, r) = quotRem n d
  {-# INLINEABLE properFraction #-}

-- | @since 0.1
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
  {-# INLINEABLE readPrec #-}

-- | @since 0.1
instance Division (Fraction Integer) where
  divide = (/)
  {-# INLINEABLE divide #-}

-- | @since 0.1
instance Division (Fraction Natural) where
  divide = (/)
  {-# INLINEABLE divide #-}

-- | @since 0.1
instance ASemigroup (Fraction Integer) where
  (.+.) = (+)
  {-# INLINEABLE (.+.) #-}

-- | @since 0.1
instance ASemigroup (Fraction Natural) where
  (.+.) = (+)
  {-# INLINEABLE (.+.) #-}

-- | @since 0.1
instance AMonoid (Fraction Integer) where
  zero = UnsafeFraction 0 1
  {-# INLINEABLE zero #-}

-- | @since 0.1
instance AMonoid (Fraction Natural) where
  zero = UnsafeFraction 0 1
  {-# INLINEABLE zero #-}

-- | @since 0.1
instance AGroup (Fraction Integer) where
  (.-.) = (-)
  {-# INLINEABLE (.-.) #-}

-- | @since 0.1
instance MSemigroup (Fraction Integer) where
  (.*.) = (*)
  {-# INLINEABLE (.*.) #-}

-- | @since 0.1
instance MSemigroup (Fraction Natural) where
  (.*.) = (*)
  {-# INLINEABLE (.*.) #-}

-- | @since 0.1
instance MMonoid (Fraction Integer) where
  one = UnsafeFraction 1 1
  {-# INLINEABLE one #-}

-- | @since 0.1
instance MMonoid (Fraction Natural) where
  one = UnsafeFraction 1 1
  {-# INLINEABLE one #-}

-- | @since 0.1
instance MGroup (Fraction Integer) where
  x .%. MkNonZero (n :%: d) = x .*. UnsafeFraction d n
  {-# INLINEABLE (.%.) #-}

-- | @since 0.1
instance MGroup (Fraction Natural) where
  x .%. MkNonZero (n :%: d) = x .*. UnsafeFraction d n
  {-# INLINEABLE (.%.) #-}

-- | @since 0.1
instance Normed (Fraction Integer) where
  norm = abs
  {-# INLINEABLE norm #-}

-- | @since 0.1
instance Normed (Fraction Natural) where
  norm = abs
  {-# INLINEABLE norm #-}

-- | @since 0.1
instance Semiring (Fraction Integer)

-- | @since 0.1
instance Semiring (Fraction Natural)

-- | @since 0.1
instance Ring (Fraction Integer)

-- | @since 0.1
instance Semifield (Fraction Natural)

-- | @since 0.1
instance Semifield (Fraction Integer)

-- | @since 0.1
instance Field (Fraction Integer)

-- | @since 0.1
instance FromInteger (Fraction Integer) where
  afromInteger = fromInteger
  {-# INLINEABLE afromInteger #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance FromInteger (Fraction Natural) where
  afromInteger = fromInteger
  {-# INLINEABLE afromInteger #-}

-- | @since 0.1
instance FromRational (Fraction Integer) where
  afromRational = fromRational
  {-# INLINEABLE afromRational #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance FromRational (Fraction Natural) where
  afromRational = fromRational
  {-# INLINEABLE afromRational #-}

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
-- @since 0.1
mkFraction :: (Integral a, UpperBoundless a) => a -> a -> Maybe (Fraction a)
mkFraction _ 0 = Nothing
mkFraction n d = Just $ reduce (UnsafeFraction n d)
{-# INLINEABLE mkFraction #-}

-- | Template haskell for creating a 'Fraction' at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkFractionTH 7 2)
-- 7 :%: 2
--
-- @since 0.1
#if MIN_VERSION_template_haskell(2,17,0)
mkFractionTH :: (Integral a, Lift a, UpperBoundless a) => a -> a -> Code Q (Fraction a)
#else
mkFractionTH :: (Integral a, Lift a, UpperBoundless a) => a -> a -> Q (TExp (Fraction a))
#endif
mkFractionTH n = maybe R.ratioZeroDenominatorError liftTyped . mkFraction n
{-# INLINEABLE mkFractionTH #-}

-- | Infix version of 'mkFractionTH'.
--
-- ==== __Examples__
--
-- >>> $$(7 %% 2)
-- 7 :%: 2
--
-- @since 0.1
#if MIN_VERSION_template_haskell(2,17,0)
(%%) :: (Integral a, Lift a, UpperBoundless a) => a -> a -> Code Q (Fraction a)
#else
(%%) :: (Integral a, Lift a, UpperBoundless a) => a -> a -> Q (TExp (Fraction a))
#endif
n %% d = mkFractionTH n d
{-# INLINE (%%) #-}

infixl 7 %%

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
-- @since 0.1
unsafeFraction :: (HasCallStack, Integral a, UpperBoundless a) => a -> a -> Fraction a
unsafeFraction n = May.fromMaybe R.ratioZeroDenominatorError . mkFraction n
{-# INLINEABLE unsafeFraction #-}

-- | Infix version of 'unsafeFraction'.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
--
-- >>> 7 %! 2
-- 7 :%: 2
--
-- >>> 7 %! 0
-- *** Exception: Ratio has zero denominator
--
-- @since 0.1
(%!) :: (Integral a, UpperBoundless a) => a -> a -> Fraction a
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
-- 7 :%: 2
--
-- >>> reduce (18 %! 10)
-- 9 :%: 5
--
-- >>> reduce (-5 %! -5)
-- 1 :%: 1
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

-- $optics
-- In addition to '_MkFraction', we have 'LabelOptic' instances for
-- "numerator" and "denominator".
--
-- ==== __Examples__
--
-- >>> :set -XOverloadedLabels
-- >>> import Optics.Core (view, set)
-- >>> let x = 2 %! 7
-- >>> view #numerator x
-- 2
--
-- >>> set #numerator 5 x
-- 5 :%: 7
--
-- >>> view #denominator x
-- 7

-- | 'ReversedPrism'' that enables total elimination and partial construction.
--
-- ==== __Examples__
--
-- >>> import Optics.Core ((^.))
-- >>> f = $$(2 %% 8)
-- >>> f ^. _MkFraction
-- (1,4)
--
-- >>> rmatching _MkFraction (0, 4)
-- Right (0 :%: 1)
--
-- >>> rmatching _MkFraction (1, 0)
-- Left (1,0)
--
-- @since 0.1
_MkFraction :: (Integral a, Ord a, UpperBoundless a) => ReversedPrism' (Fraction a) (a, a)
_MkFraction = re (prism (\(UnsafeFraction n d) -> (n, d)) g)
  where
    g x = case uncurry mkFraction x of
      Nothing -> Left x
      Just x' -> Right x'
{-# INLINEABLE _MkFraction #-}

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False
{-# INLINEABLE xor #-}

infixr 2 `xor`
