-- | Provides types for enforcing minimum and maximum bounds.
--
-- @since 0.1
module Numeric.Data.Interval.Algebra
  ( -- * Types
    IntervalBound (..),
    Interval (MkInterval),

    -- * Creation
    Internal.mkInterval,
    mkIntervalTH,
    Internal.unsafeInterval,
    reallyUnsafeInterval,

    -- * Elimination
    unInterval,

    -- * Bound aliases
    -- $bound-aliases
    O,
    C,
    N,

    -- * Optics
    -- $optics
    _MkInterval,
    rmatching,
    _Open,
    _Closed,
    _None,
  )
where

import Data.Singletons (SingI)
import GHC.TypeNats (Nat)
import Language.Haskell.TH (Code, Q)
import Language.Haskell.TH.Syntax (Lift (liftTyped))
import Numeric.Convert.Integer (FromInteger)
import Numeric.Data.Internal.Utils (rmatching)
import Numeric.Data.Interval.Algebra.Internal
  ( Interval (MkInterval, UnsafeInterval),
    IntervalBound (Closed, None, Open),
  )
import Numeric.Data.Interval.Algebra.Internal qualified as Internal
import Optics.Core (Prism', ReversedPrism', ReversibleOptic (re), prism)

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
  ( FromInteger a,
    Lift a,
    Ord a,
    SingI l,
    SingI r,
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

-- $bound-aliases
-- These aliases allow for writing interval types more concisely.
--
-- >>> Internal.unsafeInterval @(O 10) @(C 100) 50
-- UnsafeInterval (Open 10) (Closed 100) 50
--
-- >>> Internal.unsafeInterval @N @(C 100) 50
-- UnsafeInterval None (Closed 100) 50

-- | Alias for 'Open', for writing bounds more concisely.
type O :: Nat -> IntervalBound
type O n = Open n

-- | Alias for 'Closed', for writing bounds more concisely.
type C :: Nat -> IntervalBound
type C n = Closed n

-- | Alias for 'None', for writing bounds more concisely.
type N :: IntervalBound
type N = None

-- $optics
-- We provide a 'ReversedPrism'' '_MkInterval' that allows for total
-- elimination and partial construction, along with a 'Optics.Core.LabelOptic'
-- 'Optics.Core.Getter' for @#unInterval@.
--
-- ==== __Examples__
--
-- >>> :set -XOverloadedLabels
-- >>> import Optics.Core (view)
-- >>> let x = $$(mkIntervalTH @(Open 1) @(Open 5) 2)
-- >>> view #unInterval x
-- 2

-- | 'ReversedPrism'' that enables total elimination and partial construction.
--
-- ==== __Examples__
-- >>> import Optics.Core (view)
-- >>> x = $$(mkIntervalTH @(Open 1) @(Open 5) 2)
-- >>> view _MkInterval x
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
  ( FromInteger a,
    Ord a,
    SingI l,
    SingI r
  ) =>
  ReversedPrism' (Interval l r a) a
_MkInterval = re (prism unInterval g)
  where
    g x = case Internal.mkInterval x of
      Nothing -> Left x
      Just x' -> Right x'
{-# INLINEABLE _MkInterval #-}

-- | @since 0.1
_Open :: Prism' IntervalBound Nat
_Open =
  prism
    Open
    ( \case
        Open x -> Right x
        other -> Left other
    )
{-# INLINEABLE _Open #-}

-- | @since 0.1
_Closed :: Prism' IntervalBound Nat
_Closed =
  prism
    Closed
    ( \case
        Closed x -> Right x
        other -> Left other
    )
{-# INLINEABLE _Closed #-}

-- | @since 0.1
_None :: Prism' IntervalBound ()
_None =
  prism
    (const None)
    ( \case
        None -> Right ()
        other -> Left other
    )
{-# INLINEABLE _None #-}
