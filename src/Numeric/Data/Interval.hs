{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- see NOTE: [TypeAbstractions default extensions]

#if __GLASGOW_HASKELL__ >= 908
{-# LANGUAGE TypeAbstractions #-}
#endif

-- | Provides types for enforcing minimum and maximum bounds.
--
-- @since 0.1
module Numeric.Data.Interval
  ( -- * Main types
    IntervalBound (..),
    Interval (MkInterval),

    -- ** Creation,
    mkInterval,
    mkIntervalTH,
    unsafeInterval,
    reallyUnsafeInterval,

    -- ** Elimination
    unInterval,

    -- ** Optics
    _MkInterval,
    rmatching,

    -- * Singletons
    SIntervalBound (..),
    SingIntervalBound (..),
    withSingIntervalBound,
  )
where

import Control.DeepSeq (NFData)
import Data.Bounds
  ( LowerBounded (lowerBound),
    LowerBoundless,
    UpperBounded (upperBound),
    UpperBoundless,
  )
import Data.Kind (Constraint, Type)
import Data.Maybe qualified as Maybe
import Data.Proxy (Proxy (Proxy))
#if !MIN_VERSION_prettyprinter(1, 7, 1)
import Data.Text.Prettyprint.Doc (Pretty (pretty))
#endif
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat, Nat, natVal)
import Language.Haskell.TH (Code, Q)
import Language.Haskell.TH.Syntax (Lift (liftTyped))
import Numeric.Data.NonZero (rmatching)
import Numeric.Literal.Integer (FromInteger (afromInteger))
import Numeric.Literal.Rational (FromRational (afromRational))
import Optics.Core (ReversedPrism', ReversibleOptic (re), prism)
#if MIN_VERSION_prettyprinter(1, 7, 1)
import Prettyprinter (Pretty (pretty))
#endif

-- $setup
-- >>> :set -XTemplateHaskell

type IntervalBound :: Type

-- | Interval bound.
--
-- @since 0.1
data IntervalBound
  = -- | Open bound.
    Open Nat
  | -- | Closed bound.
    Closed Nat
  | -- | No bound.
    None
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
      NFData
    )

displayIntervalBounds :: IntervalBound -> IntervalBound -> String
displayIntervalBounds l r =
  mconcat
    [ bracketL l,
      valL l,
      ", ",
      valR r,
      bracketR r
    ]
  where
    valL (Open n) = show n
    valL (Closed n) = show n
    valL None = "-\8734"

    valR (Open n) = show n
    valR (Closed n) = show n
    valR None = "\8734"

    bracketL (Closed _) = "["
    bracketL _ = "("

    bracketR (Closed _) = "]"
    bracketR _ = ")"

type SIntervalBound :: IntervalBound -> Type

-- | Singleton for 'IntervalBound'.
--
-- @since 0.1
data SIntervalBound (i :: IntervalBound) where
  SOpen :: forall (n :: Nat). (KnownNat n) => SIntervalBound (Open n)
  SClosed :: forall (n :: Nat). (KnownNat n) => SIntervalBound (Closed n)
  SNone :: SIntervalBound None

-- | Singleton \"with\"-style convenience function. Allows us to run a
-- computation @SingIntervalBound i => r@ without explicitly pattern-matching
-- every time.
--
-- @since 0.1
withSingIntervalBound :: SIntervalBound i -> ((SingIntervalBound i) => r) -> r
withSingIntervalBound i x = case i of
  SOpen -> x
  SClosed -> x
  SNone -> x
{-# INLINEABLE withSingIntervalBound #-}

type SingIntervalBound :: IntervalBound -> Constraint

-- | Class for retrieving the singleton witness from the 'IntervalBound'.
--
-- @since 0.1
class SingIntervalBound (s :: IntervalBound) where
  -- | Retrieves the singleton witness.
  --
  -- @since 0.1
  singIntervalBound :: SIntervalBound s

-- | @since 0.1
instance (KnownNat k) => SingIntervalBound (Open k) where
  singIntervalBound = SOpen @k

-- | @since 0.1
instance (KnownNat k) => SingIntervalBound (Closed k) where
  singIntervalBound = SClosed @k

-- | @since 0.1
instance SingIntervalBound None where
  singIntervalBound = SNone

type Interval :: IntervalBound -> IntervalBound -> Type -> Type

-- | Represents an interval. Can be (open|closed) bounded (left|right).
--
-- @since 0.1
newtype Interval (l :: IntervalBound) (r :: IntervalBound) (a :: Type)
  = UnsafeInterval a
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
      NFData
    )

-- | @since 0.1
instance
  ( KnownNat l,
    KnownNat r,
    Num a
  ) =>
  Bounded (Interval (Closed l) (Closed r) a)
  where
  minBound = UnsafeInterval $ fromIntegral $ natVal @l Proxy
  maxBound = UnsafeInterval $ fromIntegral $ natVal @r Proxy

-- | @since 0.1
instance (KnownNat l, Num a) => LowerBounded (Interval (Closed l) r a) where
  lowerBound = UnsafeInterval $ fromIntegral $ natVal @l Proxy

-- | @since 0.1
instance (LowerBoundless a) => LowerBoundless (Interval None r a)

-- | @since 0.1
instance (KnownNat r, Num a) => UpperBounded (Interval l (Closed r) a) where
  upperBound = UnsafeInterval $ fromIntegral $ natVal @r Proxy

-- | @since 0.1
instance (UpperBoundless a) => UpperBoundless (Interval l None a)

-- | @since 0.1
instance (Pretty a) => Pretty (Interval l r a) where
  pretty (UnsafeInterval x) = pretty x
  {-# INLINEABLE pretty #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance
  ( Num a,
    Ord a,
    SingIntervalBound l,
    SingIntervalBound r,
    Show a
  ) =>
  FromInteger (Interval l r a)
  where
  afromInteger = unsafeInterval . fromInteger
  {-# INLINEABLE afromInteger #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance
  ( Fractional a,
    Ord a,
    SingIntervalBound l,
    SingIntervalBound r,
    Show a
  ) =>
  FromRational (Interval l r a)
  where
  afromRational = unsafeInterval . fromRational
  {-# INLINEABLE afromRational #-}

pattern MkInterval :: a -> Interval l r a
pattern MkInterval x <- UnsafeInterval x

{-# COMPLETE MkInterval #-}

-- | @since 0.1
unInterval :: Interval l r a -> a
unInterval (UnsafeInterval x) = x

-- | Template haskell for creating an 'Interval' at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkIntervalTH @None @(Closed 100) 7)
-- UnsafeInterval 7
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
mkIntervalTH x = maybe (error msg) liftTyped $ mkInterval x
  where
    msg = errMsg @l @r x "mkIntervalTH"
{-# INLINEABLE mkIntervalTH #-}

-- | Smart constructor for 'Interval'. Returns 'Nothing' if the given value
-- is not within the bounds.
--
-- ==== __Examples__
-- >>> mkInterval @(Open 10) @(Closed 100) 50
-- Just (UnsafeInterval 50)
--
-- >>> mkInterval @(Open 10) @(Closed 100) 100
-- Just (UnsafeInterval 100)
--
-- >>> mkInterval @(Open 10) @(Closed 100) 10
-- Nothing
--
-- >>> mkInterval @(Open 10) @(Closed 100) 101
-- Nothing
--
-- @since 0.1
mkInterval ::
  forall l r a.
  ( Num a,
    Ord a,
    SingIntervalBound l,
    SingIntervalBound r
  ) =>
  a ->
  Maybe (Interval l r a)
mkInterval x
  | boundedLeft && boundedRight = Just (UnsafeInterval x)
  | otherwise = Nothing
  where
    boundedLeft :: Bool
    boundedLeft = case singIntervalBound @l of
      SNone -> True
      (SOpen @k) ->
        let l' = natVal @k Proxy
         in x > fromIntegral l'
      (SClosed @k) ->
        let l' = natVal @k Proxy
         in x >= fromIntegral l'

    boundedRight :: Bool
    boundedRight = case singIntervalBound @r of
      SNone -> True
      (SOpen @k) ->
        let r' = natVal @k Proxy
         in x < fromIntegral r'
      (SClosed @k) ->
        let r' = natVal @k Proxy
         in x <= fromIntegral r'
{-# INLINEABLE mkInterval #-}

-- | Variant of 'mkInterval' that throws an error when given a value out of bounds.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> unsafeInterval @(Open 10) @(Closed 100) 50
-- UnsafeInterval 50
--
-- @since 0.1
unsafeInterval ::
  forall l r a.
  ( HasCallStack,
    Num a,
    Ord a,
    SingIntervalBound l,
    SingIntervalBound r,
    Show a
  ) =>
  a ->
  Interval l r a
unsafeInterval x = Maybe.fromMaybe (error msg) $ mkInterval x
  where
    msg = errMsg @l @r x "unsafeInterval"
{-# INLINEABLE unsafeInterval #-}

-- | This function is an alias for the unchecked constructor @UnsafeInterval@
-- i.e. it allows us to construct a 'Interval' __without__ checking
-- invariants. This is intended only for when we absolutely know the invariant
-- holds and a branch (i.e. 'unsafeInterval') is undesirable for performance
-- reasons. Exercise extreme caution.
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
-- Right (UnsafeInterval 3)
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
    g x = case mkInterval x of
      Nothing -> Left x
      Just x' -> Right x'
{-# INLINEABLE _MkInterval #-}

errMsg ::
  forall l r a.
  ( Show a,
    SingIntervalBound l,
    SingIntervalBound r
  ) =>
  a ->
  String ->
  String
errMsg x fnName = msg
  where
    intervalStr = displayIntervalBounds left right
    (left, right) = getInterval @l @r
    msg =
      mconcat
        [ "Numeric.Data.Interval.",
          fnName,
          ": Wanted value in ",
          intervalStr,
          ", received: ",
          show x
        ]

getInterval ::
  forall l r.
  ( SingIntervalBound l,
    SingIntervalBound r
  ) =>
  (IntervalBound, IntervalBound)
getInterval = (fromSingleton left, fromSingleton right)
  where
    left = singIntervalBound @l
    right = singIntervalBound @r

fromSingleton :: SIntervalBound i -> IntervalBound
fromSingleton SNone = None
fromSingleton (SOpen @n) = Open (natVal @n Proxy)
fromSingleton (SClosed @n) = Closed (natVal @n Proxy)
