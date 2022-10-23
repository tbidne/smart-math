{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides types for enforcing minimum and maximum bounds.
--
-- @since 0.1
module Numeric.Data.Interval
  ( -- * Left/Right-Interval

    -- ** Type
    LRInterval (MkLRInterval),

    -- ** Creation
    mkLRInterval,
    mkLRIntervalTH,
    unsafeLRInterval,
    reallyUnsafeLRInterval,

    -- ** Elimination
    unLRInterval,

    -- ** Optics
    _MkLRInterval,
    rmatching,

    -- * Left-Interval

    -- ** Type
    LInterval (MkLInterval),

    -- ** Creation
    mkLInterval,
    mkLIntervalTH,
    unsafeLInterval,
    reallyUnsafeLInterval,

    -- ** Elimination
    unLInterval,

    -- ** Optics
    _MkLInterval,

    -- * Right-Interval

    -- ** Type
    RInterval (MkRInterval),

    -- ** Creation
    mkRInterval,
    mkRIntervalTH,
    unsafeRInterval,
    reallyUnsafeRInterval,

    -- ** Elimination
    unRInterval,

    -- ** Optics
    _MkRInterval,
  )
where

import Control.DeepSeq (NFData)
import Data.Bounds
  ( LowerBounded (lowerBound),
    LowerBoundless,
    UpperBounded (upperBound),
    UpperBoundless,
  )
import Data.Kind (Type)
import Data.Maybe qualified as Maybe
import Data.Proxy (Proxy (..))
#if !MIN_VERSION_prettyprinter(1, 7, 1)
import Data.Text.Prettyprint.Doc (Pretty (..))
#endif
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat, Nat, natVal)
#if MIN_VERSION_template_haskell(2, 17, 0)
import Language.Haskell.TH (Code, Q)
#else
import Language.Haskell.TH (Q, TExp)
#endif
import Language.Haskell.TH.Syntax (Lift (..))
import Numeric.Data.NonZero (rmatching)
import Numeric.Literal.Integer (FromInteger (..))
import Numeric.Literal.Rational (FromRational (..))
import Optics.Core (ReversedPrism', ReversibleOptic (re), prism)
#if MIN_VERSION_prettyprinter(1, 7, 1)
import Prettyprinter (Pretty (..))
#endif

-- $setup
-- >>> :set -XTemplateHaskell

-- | Represents a closed interval that is bounded on both sides i.e.
-- @LRInterval \@l \@r x@ represents \( x \in [l, r] \).
--
-- @since 0.1
type LRInterval :: Nat -> Nat -> Type -> Type
newtype LRInterval l r a = UnsafeLRInterval a
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
      LowerBounded,
      -- | @since 0.1
      NFData,
      -- | @since 0.1
      UpperBounded
    )

-- | @since 0.1
unLRInterval :: LRInterval l r a -> a
unLRInterval (UnsafeLRInterval x) = x
{-# INLINE unLRInterval #-}

-- | Unidirectional pattern synonym for 'LRInterval'. This allows us to pattern
-- match on an interval term without exposing the unsafe internal details.
--
-- @since 0.1
pattern MkLRInterval :: a -> LRInterval l r a
pattern MkLRInterval x <- UnsafeLRInterval x

{-# COMPLETE MkLRInterval #-}

-- | @since 0.1
instance (KnownNat l, KnownNat r, Num a) => Bounded (LRInterval l r a) where
  minBound = UnsafeLRInterval $ fromIntegral $ natVal @l Proxy
  maxBound = UnsafeLRInterval $ fromIntegral $ natVal @r Proxy
  {-# INLINEABLE minBound #-}
  {-# INLINEABLE maxBound #-}

-- | @since 0.1
instance Pretty a => Pretty (LRInterval l r a) where
  pretty (UnsafeLRInterval x) = pretty x
  {-# INLINEABLE pretty #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance (KnownNat l, KnownNat r, Num a, Ord a, Show a) => FromInteger (LRInterval l r a) where
  afromInteger = unsafeLRInterval . fromInteger
  {-# INLINEABLE afromInteger #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance (Fractional a, KnownNat l, KnownNat r, Ord a, Show a) => FromRational (LRInterval l r a) where
  afromRational = unsafeLRInterval . fromRational
  {-# INLINEABLE afromRational #-}

-- | Template haskell for creating an 'LRInterval' at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkLRIntervalTH @0 @100 7)
-- UnsafeLRInterval 7
--
-- @since 0.1
#if MIN_VERSION_template_haskell(2,17,0)
mkLRIntervalTH ::
  forall l r a.
  (Integral a, KnownNat l, KnownNat r, Lift a, Show a) =>
  a ->
  Code Q (LRInterval l r a)
#else
mkLRIntervalTH ::
  forall l r a.
  (Integral a, KnownNat l, KnownNat r, Lift a, Show a) =>
  a ->
  Q (TExp (LRInterval l r a))
#endif
mkLRIntervalTH x = maybe (error msg) liftTyped $ mkLRInterval x
  where
    msg = lrErrMsg @l @r x "mkLRIntervalTH"
{-# INLINEABLE mkLRIntervalTH #-}

-- | Smart constructor for 'LRInterval'. Returns 'Nothing' if the given value
-- is not within the bounds.
--
-- ==== __Examples__
-- >>> mkLRInterval @10 @100 50
-- Just (UnsafeLRInterval 50)
--
-- >>> mkLRInterval @10 @100 5
-- Nothing
--
-- >>> mkLRInterval @10 @100 200
-- Nothing
--
-- @since 0.1
mkLRInterval ::
  forall l r a.
  (KnownNat l, KnownNat r, Num a, Ord a) =>
  a ->
  Maybe (LRInterval l r a)
mkLRInterval x
  | x >= l' && x <= r' = Just (UnsafeLRInterval x)
  | otherwise = Nothing
  where
    l' = fromIntegral $ natVal @l Proxy
    r' = fromIntegral $ natVal @r Proxy
{-# INLINEABLE mkLRInterval #-}

-- | Variant of 'mkLRInterval' that throws an error when given a value out of
-- bounds.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> unsafeLRInterval @0 @10 7
-- UnsafeLRInterval 7
--
-- @since 0.1
unsafeLRInterval ::
  forall l r a.
  (HasCallStack, KnownNat l, KnownNat r, Num a, Ord a, Show a) =>
  a ->
  LRInterval l r a
unsafeLRInterval x = Maybe.fromMaybe (error msg) $ mkLRInterval x
  where
    msg = lrErrMsg @l @r x "unsafeLRInterval"
{-# INLINEABLE unsafeLRInterval #-}

-- | This function is an alias for the unchecked constructor @UnsafeLRInterval@
-- i.e. it allows us to construct a 'LRInterval' __without__ checking the
-- invariant. This is intended only for when we absolutely know the invariant
-- holds and a branch (i.e. 'unsafeLRInterval') is undesirable for performance
-- reasons. Exercise extreme caution.
--
-- @since 0.1
reallyUnsafeLRInterval :: a -> LRInterval l r a
reallyUnsafeLRInterval = UnsafeLRInterval
{-# INLINEABLE reallyUnsafeLRInterval #-}

-- | 'ReversedPrism'' that enables total elimination and partial construction.
--
-- ==== __Examples__
--
-- >>> import Optics.Core ((^.))
-- >>> x = $$(mkLRIntervalTH @1 @5 2)
-- >>> x ^. _MkLRInterval
-- 2
--
-- >>> rmatching (_MkLRInterval @1 @5) 3
-- Right (UnsafeLRInterval 3)
--
-- >>> rmatching (_MkLRInterval @1 @5) 7
-- Left 7
--
-- @since 0.1
_MkLRInterval :: (KnownNat l, KnownNat r, Num a, Ord a) => ReversedPrism' (LRInterval l r a) a
_MkLRInterval = re (prism unLRInterval g)
  where
    g x = case mkLRInterval x of
      Nothing -> Left x
      Just x' -> Right x'
{-# INLINEABLE _MkLRInterval #-}

-- | Represents a closed interval that is left-bounded i.e.
-- @LInterval \@l x@ represents \( x \in [l, \infty) \).
--
-- @since 0.1
type LInterval :: Nat -> Type -> Type
newtype LInterval l a = UnsafeLInterval a
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
unLInterval :: LInterval l a -> a
unLInterval (UnsafeLInterval x) = x
{-# INLINE unLInterval #-}

-- | Unidirectional pattern synonym for 'LInterval'. This allows us to pattern
-- match on an interval term without exposing the unsafe internal details.
--
-- @since 0.1
pattern MkLInterval :: a -> LInterval l a
pattern MkLInterval x <- UnsafeLInterval x

{-# COMPLETE MkLInterval #-}

-- | @since 0.1
instance (KnownNat l, Num a) => LowerBounded (LInterval l a) where
  lowerBound = UnsafeLInterval $ fromIntegral $ natVal @l Proxy
  {-# INLINEABLE lowerBound #-}

-- | @since 0.1
instance UpperBoundless a => UpperBoundless (LInterval l a)

-- | @since 0.1
instance Pretty a => Pretty (LInterval l a) where
  pretty (UnsafeLInterval x) = pretty x
  {-# INLINEABLE pretty #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance (KnownNat l, Num a, Ord a, Show a) => FromInteger (LInterval l a) where
  afromInteger = unsafeLInterval . fromInteger
  {-# INLINEABLE afromInteger #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance (Fractional a, KnownNat l, Ord a, Show a) => FromRational (LInterval l a) where
  afromRational = unsafeLInterval . fromRational
  {-# INLINEABLE afromRational #-}

-- | Template haskell for creating a 'LInterval' at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkLIntervalTH @0 7)
-- UnsafeLInterval 7
--
-- @since 0.1
#if MIN_VERSION_template_haskell(2,17,0)
mkLIntervalTH ::
  forall l a.
  (Integral a, KnownNat l, Lift a, Show a) =>
  a ->
  Code Q (LInterval l a)
#else
mkLIntervalTH ::
  forall l a.
  (Integral a, KnownNat l, Lift a, Show a) =>
  a ->
  Q (TExp (LInterval l a))
#endif
mkLIntervalTH x = maybe (error msg) liftTyped $ mkLInterval x
  where
    msg = lErrMsg @l x "mkLIntervalTH"
{-# INLINEABLE mkLIntervalTH #-}

-- | Smart constructor for 'LInterval'. Returns 'Nothing' if the given value
-- is not >= the bound.
--
-- ==== __Examples__
-- >>> mkLInterval @10 50
-- Just (UnsafeLInterval 50)
--
-- >>> mkLInterval @10 5
-- Nothing
--
-- @since 0.1
mkLInterval ::
  forall l a.
  (KnownNat l, Num a, Ord a) =>
  a ->
  Maybe (LInterval l a)
mkLInterval x
  | x >= l' = Just (UnsafeLInterval x)
  | otherwise = Nothing
  where
    l' = fromIntegral $ natVal @l Proxy
{-# INLINEABLE mkLInterval #-}

-- | Variant of 'mkLInterval' that throws an error when given a value out of bounds.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> unsafeLInterval @0 7
-- UnsafeLInterval 7
--
-- @since 0.1
unsafeLInterval ::
  forall l a.
  (HasCallStack, KnownNat l, Num a, Ord a, Show a) =>
  a ->
  LInterval l a
unsafeLInterval x = Maybe.fromMaybe (error msg) $ mkLInterval x
  where
    msg = lErrMsg @l x "unsafeLInterval"
{-# INLINEABLE unsafeLInterval #-}

-- | This function is an alias for the unchecked constructor @UnsafeLInterval@
-- i.e. it allows us to construct a 'LInterval' __without__ checking the
-- invariant. This is intended only for when we absolutely know the invariant
-- holds and a branch (i.e. 'unsafeLInterval') is undesirable for performance
-- reasons. Exercise extreme caution.
--
-- @since 0.1
reallyUnsafeLInterval :: a -> LInterval l a
reallyUnsafeLInterval = UnsafeLInterval
{-# INLINEABLE reallyUnsafeLInterval #-}

-- | 'ReversedPrism'' that enables total elimination and partial construction.
--
-- ==== __Examples__
--
-- >>> import Optics.Core ((^.))
-- >>> x = $$(mkLIntervalTH @8 10)
-- >>> x ^. _MkLInterval
-- 10
--
-- >>> rmatching (_MkLInterval @8) 10
-- Right (UnsafeLInterval 10)
--
-- >>> rmatching (_MkLInterval @8) 5
-- Left 5
--
-- @since 0.1
_MkLInterval :: (KnownNat l, Num a, Ord a) => ReversedPrism' (LInterval l a) a
_MkLInterval = re (prism unLInterval g)
  where
    g x = case mkLInterval x of
      Nothing -> Left x
      Just x' -> Right x'
{-# INLINEABLE _MkLInterval #-}

-- | Represents a closed interval that is right-bounded i.e.
-- @RInterval \@r x@ represents \( x \in (-\infty, r] \).
--
-- @since 0.1
type RInterval :: Nat -> Type -> Type
newtype RInterval r a = UnsafeRInterval a
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
unRInterval :: RInterval r a -> a
unRInterval (UnsafeRInterval x) = x
{-# INLINE unRInterval #-}

-- | @since 0.1
instance (KnownNat r, Num a) => UpperBounded (RInterval r a) where
  upperBound = UnsafeRInterval $ fromIntegral $ natVal @r Proxy
  {-# INLINEABLE upperBound #-}

-- | @since 0.1
instance LowerBoundless a => LowerBoundless (RInterval r a)

-- | Unidirectional pattern synonym for 'RInterval'. This allows us to pattern
-- match on an interval term without exposing the unsafe internal details.
--
-- @since 0.1
pattern MkRInterval :: a -> RInterval r a
pattern MkRInterval x <- UnsafeRInterval x

{-# COMPLETE MkRInterval #-}

-- | @since 0.1
instance Pretty a => Pretty (RInterval r a) where
  pretty (UnsafeRInterval x) = pretty x
  {-# INLINEABLE pretty #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance (KnownNat r, Num a, Ord a, Show a) => FromInteger (RInterval r a) where
  afromInteger = unsafeRInterval . fromInteger
  {-# INLINEABLE afromInteger #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance (Fractional a, KnownNat r, Ord a, Show a) => FromRational (RInterval r a) where
  afromRational = unsafeRInterval . fromRational
  {-# INLINEABLE afromRational #-}

-- | Template haskell for creating an 'RInterval' at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkRIntervalTH @100 7)
-- UnsafeRInterval 7
--
-- @since 0.1
#if MIN_VERSION_template_haskell(2,17,0)
mkRIntervalTH ::
  forall r a.
  (Integral a, KnownNat r, Lift a, Show a) =>
  a ->
  Code Q (RInterval r a)
#else
mkRIntervalTH ::
  forall r a.
  (Integral a, KnownNat r, Lift a, Show a) =>
  a ->
  Q (TExp (RInterval r a))
#endif
mkRIntervalTH x = maybe (error msg) liftTyped $ mkRInterval x
  where
    msg = rErrMsg @r x "mkRIntervalTH"
{-# INLINEABLE mkRIntervalTH #-}

-- | Smart constructor for 'RInterval'. Returns 'Nothing' if the given value
-- is not <= the bound.
--
-- ==== __Examples__
-- >>> mkRInterval @100 50
-- Just (UnsafeRInterval 50)
--
-- >>> mkRInterval @0 5
-- Nothing
--
-- @since 0.1
mkRInterval ::
  forall r a.
  (KnownNat r, Num a, Ord a) =>
  a ->
  Maybe (RInterval r a)
mkRInterval x
  | x <= r' = Just (UnsafeRInterval x)
  | otherwise = Nothing
  where
    r' = fromIntegral $ natVal @r Proxy
{-# INLINEABLE mkRInterval #-}

-- | Variant of 'mkRInterval' that throws an error when given a value out of bounds.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> unsafeRInterval @10 7
-- UnsafeRInterval 7
--
-- @since 0.1
unsafeRInterval ::
  forall r a.
  (HasCallStack, KnownNat r, Num a, Ord a, Show a) =>
  a ->
  RInterval r a
unsafeRInterval x = Maybe.fromMaybe (error msg) $ mkRInterval x
  where
    msg = rErrMsg @r x "unsafeRInterval"
{-# INLINEABLE unsafeRInterval #-}

-- | This function is an alias for the unchecked constructor @UnsafeRInterval@
-- i.e. it allows us to construct a 'RInterval' __without__ checking the
-- invariant. This is intended only for when we absolutely know the invariant
-- holds and a branch (i.e. 'unsafeRInterval') is undesirable for performance
-- reasons. Exercise extreme caution.
--
-- @since 0.1
reallyUnsafeRInterval :: a -> RInterval r a
reallyUnsafeRInterval = UnsafeRInterval
{-# INLINEABLE reallyUnsafeRInterval #-}

-- | 'ReversedPrism'' that enables total elimination and partial construction.
--
-- ==== __Examples__
--
-- >>> import Optics.Core ((^.))
-- >>> x = $$(mkRIntervalTH @8 5)
-- >>> x ^. _MkRInterval
-- 5
--
-- >>> rmatching (_MkRInterval @8) 5
-- Right (UnsafeRInterval 5)
--
-- >>> rmatching (_MkRInterval @8) 10
-- Left 10
--
-- @since 0.1
_MkRInterval :: (KnownNat r, Num a, Ord a) => ReversedPrism' (RInterval r a) a
_MkRInterval = re (prism unRInterval g)
  where
    g x = case mkRInterval x of
      Nothing -> Left x
      Just x' -> Right x'
{-# INLINEABLE _MkRInterval #-}

lrErrMsg :: forall l r a. (KnownNat l, KnownNat r, Show a) => a -> String -> String
lrErrMsg x fnName = header <> msg
  where
    header = "Numeric.Data.Interval." <> fnName
    l' = natVal @l Proxy
    r' = natVal @r Proxy
    msg =
      ": Wanted value in ["
        <> show l'
        <> ", "
        <> show r'
        <> "], received: "
        <> show x
{-# INLINEABLE lrErrMsg #-}

lErrMsg :: forall l a. (KnownNat l, Show a) => a -> String -> String
lErrMsg x fnName = header <> msg
  where
    header = "Numeric.Data.Interval." <> fnName
    l' = natVal @l Proxy
    msg =
      ": Wanted value in ["
        <> show l'
        <> ", \8734), received: "
        <> show x
{-# INLINEABLE lErrMsg #-}

rErrMsg :: forall l a. (KnownNat l, Show a) => a -> String -> String
rErrMsg x fnName = header <> msg
  where
    header = "Numeric.Data.Interval." <> fnName
    l' = natVal @l Proxy
    msg =
      ": Wanted value in (-\8734, "
        <> show l'
        <> "], received: "
        <> show x
{-# INLINEABLE rErrMsg #-}
