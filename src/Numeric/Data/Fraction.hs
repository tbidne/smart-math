{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides the 'Fraction' type, a safer alternative to 'GHC.Real.Ratio'.
--
-- @since 0.1
module Numeric.Data.Fraction
  ( -- * Type
    Fraction ((:%:)),

    -- * Creation
    mkFraction,
    mkFractionTH,
    (%%),
    Internal.unsafeFraction,
    (Internal.%!),

    -- * Elimination
    Internal.numerator,
    Internal.denominator,

    -- * Functions
    reduce,

    -- * Optics
    -- $optics
    _MkFraction,
    rmatching,
  )
where

import Data.Bounds
  ( UpperBoundless,
  )
import Language.Haskell.TH (Code, Q)
import Language.Haskell.TH.Syntax (Lift (liftTyped))
import Numeric.Data.Fraction.Internal (Fraction (UnsafeFraction, (:%:)))
import Numeric.Data.Fraction.Internal qualified as Internal
import Numeric.Data.Internal.Utils (rmatching)
import Optics.Core
  ( ReversedPrism',
    ReversibleOptic (re),
    prism,
  )

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> import Numeric.Data.Fraction.Internal ((%!))

-- | Template haskell for creating a 'Fraction' at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkFractionTH 7 2)
-- UnsafeFraction 7 2
--
-- @since 0.1
mkFractionTH ::
  ( Integral a,
    Lift a,
    UpperBoundless a
  ) =>
  a ->
  a ->
  Code Q (Fraction a)
mkFractionTH n = maybe (error err) liftTyped . mkFraction n
  where
    err = Internal.errMsg "mkFractionTH"
{-# INLINEABLE mkFractionTH #-}

-- | Smart constructor for 'Fraction'. Returns 'Nothing' if the second
-- parameter is 0. Reduces the fraction via 'reduce' if possible.
--
-- ==== __Examples__
-- >>> mkFraction 10 4
-- Just (UnsafeFraction 5 2)
--
-- >>> mkFraction 10 0
-- Nothing
--
-- @since 0.1
mkFraction :: (Integral a, UpperBoundless a) => a -> a -> Maybe (Fraction a)
mkFraction _ 0 = Nothing
mkFraction n d = Just $ reduce (UnsafeFraction n d)
{-# INLINEABLE mkFraction #-}

-- | Infix version of 'mkFractionTH'.
--
-- ==== __Examples__
--
-- >>> $$(7 %% 2)
-- UnsafeFraction 7 2
--
-- @since 0.1
(%%) :: (Integral a, Lift a, UpperBoundless a) => a -> a -> Code Q (Fraction a)
n %% d = mkFractionTH n d
{-# INLINE (%%) #-}

infixl 7 %%

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

-- $optics
-- We provide a 'ReversedPrism'' '_MkFraction' that allows for total
-- elimination and partial construction, along with 'Optics.Core.LabelOptic' instances for
-- "numerator" and "denominator".
--
-- ==== __Examples__
--
-- >>> :set -XOverloadedLabels
-- >>> import Optics.Core (set, view)
-- >>> let x = 2 %! 7
-- >>> view #numerator x
-- 2
--
-- >>> set #numerator 5 x
-- UnsafeFraction 5 7
--
-- >>> view #denominator x
-- 7

-- | 'ReversedPrism'' that enables total elimination and partial construction.
--
-- ==== __Examples__
-- >>> import Optics.Core (view)
-- >>> f = $$(2 %% 8)
-- >>> view _MkFraction f
-- (1,4)
--
-- >>> rmatching _MkFraction (0, 4)
-- Right (UnsafeFraction 0 1)
--
-- >>> rmatching _MkFraction (1, 0)
-- Left (1,0)
--
-- @since 0.1
_MkFraction ::
  ( Integral a,
    Ord a,
    UpperBoundless a
  ) =>
  ReversedPrism' (Fraction a) (a, a)
_MkFraction = re (prism (\(UnsafeFraction n d) -> (n, d)) g)
  where
    g x = case uncurry mkFraction x of
      Nothing -> Left x
      Just x' -> Right x'
{-# INLINEABLE _MkFraction #-}
