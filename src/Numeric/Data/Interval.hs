-- | Provides types for enforcing minimum and maximum bounds.
--
-- @since 0.1
module Numeric.Data.Interval
  ( -- * Main types
    IntervalBound (..),
    Interval (MkInterval),

    -- ** Creation
    Internal.mkInterval,
    mkIntervalTH,
    Internal.unsafeInterval,
    reallyUnsafeInterval,

    -- ** Elimination
    unInterval,

    -- ** Optics
    _MkInterval,
    rmatching,
  )
where

import Language.Haskell.TH (Code, Q)
import Language.Haskell.TH.Syntax (Lift (liftTyped))
import Numeric.Data.Internal.Utils (rmatching)
import Numeric.Data.Interval.Internal
  ( Interval (MkInterval, UnsafeInterval),
    IntervalBound (Closed, None, Open),
    SingIntervalBound,
  )
import Numeric.Data.Interval.Internal qualified as Internal
import Optics.Core (ReversedPrism', ReversibleOptic (re), prism)

-- $setup
-- >>> :set -XTemplateHaskell

-- | @since 0.1
unInterval :: Interval l r a -> a
unInterval (UnsafeInterval x) = x

-- | Template haskell for creating an 'Interval' at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkIntervalTH @None @(Closed 100) 7)
-- UnsafeInterval None (Closed 100) 7
--
-- @since 0.1
mkIntervalTH ::
  forall l r a.
  ( Lift a,
    Num a,
    Ord a,
    SingIntervalBound l,
    SingIntervalBound r,
    Show a
  ) =>
  a ->
  Code Q (Interval l r a)
mkIntervalTH x = maybe (error msg) liftTyped $ Internal.mkInterval x
  where
    msg = Internal.errMsg @l @r x "mkIntervalTH"
{-# INLINEABLE mkIntervalTH #-}

-- | This function is an alias for the unchecked constructor @UnsafeInterval@
-- i.e. it allows us to construct a 'Interval' __without__ checking
-- invariants. This is intended only for when we absolutely know the invariant
-- holds and a branch (i.e. 'Internal.unsafeInterval') is undesirable for
-- performance reasons. Exercise extreme caution.
--
-- @since 0.1
reallyUnsafeInterval :: a -> Interval l r a
reallyUnsafeInterval = UnsafeInterval
{-# INLINEABLE reallyUnsafeInterval #-}

-- | 'ReversedPrism'' that enables total elimination and partial construction.
--
-- ==== __Examples__
--
-- >>> import Optics.Core ((^.))
-- >>> x = $$(mkIntervalTH @(Open 1) @(Open 5) 2)
-- >>> x ^. _MkInterval
-- 2
--
-- >>> rmatching (_MkInterval @(Open 1) @(Open 5)) 3
-- Right (UnsafeInterval (Open 1) (Open 5) 3)
--
-- >>> rmatching (_MkInterval @(Open 1) @(Open 5)) 7
-- Left 7
--
-- @since 0.1
_MkInterval ::
  forall l r a.
  ( Num a,
    Ord a,
    SingIntervalBound l,
    SingIntervalBound r
  ) =>
  ReversedPrism' (Interval l r a) a
_MkInterval = re (prism unInterval g)
  where
    g x = case Internal.mkInterval x of
      Nothing -> Left x
      Just x' -> Right x'
{-# INLINEABLE _MkInterval #-}
