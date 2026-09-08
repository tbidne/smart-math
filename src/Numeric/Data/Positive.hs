-- | Provides the 'Positive' type for enforcing a positive invariant.
--
-- @since 0.1
module Numeric.Data.Positive
  ( -- * Type
    Positive (MkPositive),

    -- * Creation
    mkPositiveTH,
    mkPositive,
    Internal.unsafePositive,
    (+!),
    reallyUnsafePositive,

    -- * Elimination
    unPositive,

    -- * Functions
    positiveToNonZero,
    unsafeLiftPositive,
    unsafeLiftPositive2,
    unsafeLiftPositive3,

    -- * Optics
    -- $optics
    _MkPositive,
    rmatching,
  )
where

import Data.Bifunctor (Bifunctor (first))
import GHC.Stack (HasCallStack)
import Language.Haskell.TH (Code, Q)
import Language.Haskell.TH.Syntax (Lift)
import Numeric.Algebra.Additive.AMonoid (AMonoid (zero))
import Numeric.Data.Internal.Utils qualified as Utils
import Numeric.Data.NonZero (NonZero, reallyUnsafeNonZero, rmatching)
import Numeric.Data.Positive.Internal (Positive (MkPositive, UnsafePositive))
import Numeric.Data.Positive.Internal qualified as Internal
import Optics.Core
  ( ReversedPrism',
    ReversibleOptic (re),
    prism,
  )

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> :set -XPostfixOperators
-- >>> import Numeric.Data.Positive.Internal (unsafePositive)

-- | Template haskell for creating a 'Positive' at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkPositiveTH 1)
-- UnsafePositive 1
--
-- @since 0.1
mkPositiveTH :: (AMonoid a, Lift a, Ord a, Show a) => a -> Code Q (Positive a)
mkPositiveTH = Utils.liftErrorTH . mkPositive
{-# INLINEABLE mkPositiveTH #-}

-- | Smart constructor for 'Positive'. Returns 'Nothing' if the second
-- parameter is @<= 0@.
--
-- ==== __Examples__
-- >>> mkPositive 7
-- Right (UnsafePositive 7)
--
-- >>> mkPositive 0
-- Left "Numeric.Data.Positive: Received value <= zero: 0"
--
-- @since 0.1
mkPositive :: (AMonoid a, Ord a, Show a) => a -> Either String (Positive a)
mkPositive x
  | x > zero = Right (UnsafePositive x)
  | otherwise = Left $ Internal.errMsg x
{-# INLINEABLE mkPositive #-}

-- | Postfix operator for 'Internal.unsafePositive'.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
--
-- >>> (7 +!)
-- UnsafePositive 7
--
-- @since 0.1
(+!) :: (AMonoid a, HasCallStack, Ord a, Show a) => a -> Positive a
(+!) = Internal.unsafePositive
{-# INLINE (+!) #-}

infixl 7 +!

-- | This function is an alias for the unchecked constructor @UnsafePositive@
-- i.e. it allows us to construct a 'Positive' __without__ checking the
-- invariant. This is intended only for when we absolutely know the invariant
-- holds and a branch (i.e. 'Internal.unsafePositive') is undesirable for
-- performance reasons. Exercise extreme caution.
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

-- $optics
-- We provide a 'ReversedPrism'' '_MkPositive' that allows for total
-- elimination and partial construction, along with a 'Optics.Core.LabelOptic' 'Optics.Core.Getter'
-- for @#unPositive@.
--
-- ==== __Examples__
--
-- >>> :set -XOverloadedLabels
-- >>> import Optics.Core (view)
-- >>> let n = $$(mkPositiveTH 2)
-- >>> view #unPositive n
-- 2

-- | 'ReversedPrism'' that enables total elimination and partial construction.
--
-- ==== __Examples__
-- >>> import Optics.Core (view)
-- >>> pos = $$(mkPositiveTH 2)
-- >>> view _MkPositive pos
-- 2
--
-- >>> rmatching _MkPositive 3
-- Right (UnsafePositive 3)
--
-- >>> rmatching _MkPositive 0
-- Left 0
--
-- @since 0.1
_MkPositive :: (AMonoid a, Ord a, Show a) => ReversedPrism' (Positive a) a
_MkPositive = re (prism unPositive g)
  where
    g x = first (const x) . mkPositive $ x
{-# INLINEABLE _MkPositive #-}

-- | Lifts an unsafe unary function onto a 'Positive'.
--
-- @since 0.1
unsafeLiftPositive ::
  ( AMonoid b,
    HasCallStack,
    Ord b,
    Show b
  ) =>
  (a -> b) ->
  Positive a ->
  Positive b
unsafeLiftPositive f (UnsafePositive x) = Internal.unsafePositive (f x)
{-# INLINEABLE unsafeLiftPositive #-}

-- | Lifts an unsafe binary function onto a 'Positive'.
--
-- @since 0.1
unsafeLiftPositive2 ::
  ( AMonoid c,
    HasCallStack,
    Ord c,
    Show c
  ) =>
  (a -> b -> c) ->
  Positive a ->
  Positive b ->
  Positive c
unsafeLiftPositive2 f (UnsafePositive x1) (UnsafePositive x2) =
  Internal.unsafePositive (f x1 x2)
{-# INLINEABLE unsafeLiftPositive2 #-}

-- | Lifts an unsafe 3-ary function onto a 'Positive'.
--
-- @since 0.1
unsafeLiftPositive3 ::
  ( AMonoid d,
    HasCallStack,
    Ord d,
    Show d
  ) =>
  (a -> b -> c -> d) ->
  Positive a ->
  Positive b ->
  Positive c ->
  Positive d
unsafeLiftPositive3 f (UnsafePositive x1) (UnsafePositive x2) (UnsafePositive x3) =
  Internal.unsafePositive (f x1 x2 x3)
{-# INLINEABLE unsafeLiftPositive3 #-}
