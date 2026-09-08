{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides the 'Fraction' type, a safer alternative to 'GHC.Real.Ratio'.
--
-- @since 0.1
module Numeric.Data.Fraction
  ( -- * Type
    Fraction ((:%:), (:%!)),

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
    Internal.reduce,
    unsafeLiftFraction,
    unsafeLiftFraction2,
    unsafeLiftFraction3,

    -- * Optics
    -- $optics
    _MkFraction,
    rmatching,
  )
where

import Data.Bifunctor (Bifunctor (first))
import Data.Bounds
  ( UpperBoundless,
  )
import GHC.Stack.Types (HasCallStack)
import Language.Haskell.TH (Code, Q)
import Language.Haskell.TH.Syntax (Lift)
import Numeric.Algebra.Additive.AMonoid (pattern NonZero, pattern Zero)
import Numeric.Algebra.Multiplicative.MEuclidean (MEuclidean)
import Numeric.Algebra.Normed (Normed)
import Numeric.Algebra.Rings.Semiring (Semiring)
import Numeric.Data.Fraction.Internal
  ( Fraction
      ( UnsafeFraction,
        (:%!),
        (:%:)
      ),
  )
import Numeric.Data.Fraction.Internal qualified as Internal
import Numeric.Data.Internal.Utils (rmatching)
import Numeric.Data.Internal.Utils qualified as Utils
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
  ( Lift a,
    MEuclidean a,
    Normed a,
    Ord a,
    Semiring a,
    UpperBoundless a
  ) =>
  a ->
  a ->
  Code Q (Fraction a)
mkFractionTH n = Utils.liftErrorTH . mkFraction n
{-# INLINEABLE mkFractionTH #-}

-- | Smart constructor for 'Fraction'. Returns 'Nothing' if the second
-- parameter is 0. Reduces the fraction via 'reduce' if possible.
--
-- ==== __Examples__
-- >>> mkFraction 10 4
-- Right (UnsafeFraction 5 2)
--
-- >>> mkFraction 10 0
-- Left "Numeric.Data.Fraction: Fraction has zero denominator"
--
-- @since 0.1
mkFraction ::
  ( MEuclidean a,
    Normed a,
    Ord a,
    Semiring a,
    UpperBoundless a
  ) =>
  a ->
  a ->
  Either String (Fraction a)
mkFraction _ Zero = Left Internal.errMsg
mkFraction n (NonZero d) = Right $ Internal.reduce (UnsafeFraction n d)
{-# INLINEABLE mkFraction #-}

-- | Infix version of 'mkFractionTH'.
--
-- ==== __Examples__
--
-- >>> $$(7 %% 2)
-- UnsafeFraction 7 2
--
-- @since 0.1
(%%) ::
  ( Lift a,
    MEuclidean a,
    Normed a,
    Ord a,
    Semiring a,
    UpperBoundless a
  ) =>
  a ->
  a ->
  Code Q (Fraction a)
n %% d = mkFractionTH n d
{-# INLINE (%%) #-}

infixl 7 %%

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
  ( MEuclidean a,
    Normed a,
    Ord a,
    Semiring a,
    UpperBoundless a
  ) =>
  ReversedPrism' (Fraction a) (a, a)
_MkFraction = re (prism (\(UnsafeFraction n d) -> (n, d)) g)
  where
    g x = first (const x) . uncurry mkFraction $ x
{-# INLINEABLE _MkFraction #-}

-- | Lifts an unsafe unary function onto a 'Fraction'.
--
-- @since 0.1
unsafeLiftFraction ::
  ( HasCallStack,
    MEuclidean b,
    Normed b,
    Ord b,
    Semiring b,
    UpperBoundless b
  ) =>
  ((a, a) -> (b, b)) ->
  Fraction a ->
  Fraction b
unsafeLiftFraction f (UnsafeFraction n d) =
  let (n2, d2) = f (n, d)
   in Internal.unsafeFraction n2 d2
{-# INLINEABLE unsafeLiftFraction #-}

-- | Lifts an unsafe binary function onto a 'Fraction'.
--
-- @since 0.1
unsafeLiftFraction2 ::
  ( HasCallStack,
    MEuclidean c,
    Normed c,
    Ord c,
    Semiring c,
    UpperBoundless c
  ) =>
  ((a, a) -> (b, b) -> (c, c)) ->
  Fraction a ->
  Fraction b ->
  Fraction c
unsafeLiftFraction2 f (UnsafeFraction n1 d1) (UnsafeFraction n2 d2) =
  let (n3, d3) = f (n1, d1) (n2, d2)
   in Internal.unsafeFraction n3 d3
{-# INLINEABLE unsafeLiftFraction2 #-}

-- | Lifts an unsafe 3-ary function onto a 'Fraction'.
--
-- @since 0.1
unsafeLiftFraction3 ::
  ( HasCallStack,
    MEuclidean d,
    Normed d,
    Ord d,
    Semiring d,
    UpperBoundless d
  ) =>
  ((a, a) -> (b, b) -> (c, c) -> (d, d)) ->
  Fraction a ->
  Fraction b ->
  Fraction c ->
  Fraction d
unsafeLiftFraction3 f (UnsafeFraction n1 d1) (UnsafeFraction n2 d2) (UnsafeFraction n3 d3) =
  let (n4, d4) = f (n1, d1) (n2, d2) (n3, d3)
   in Internal.unsafeFraction n4 d4
{-# INLINEABLE unsafeLiftFraction3 #-}
