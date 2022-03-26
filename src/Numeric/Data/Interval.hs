{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Provides types for enforcing minimum and maximum bounds.
--
-- @since 0.1.0.0
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
  )
where

import Control.DeepSeq (NFData)
import Data.Kind (Type)
import Data.Maybe qualified as Maybe
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat, Nat, natVal)
#if MIN_VERSION_template_haskell(2, 17, 0)
import Language.Haskell.TH (Code, Q)
#else
import Language.Haskell.TH (Q, TExp)
#endif
import Language.Haskell.TH.Syntax (Lift (..))

-- | Represents a closed interval that is bounded on both sides i.e.
-- @LRInterval \@l \@r x@ represents \( x \in [l, r] \).
--
-- @since 0.1.0.0
type LRInterval :: Nat -> Nat -> Type -> Type
newtype LRInterval l r a = UnsafeLRInterval a
  deriving stock
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Generic,
      -- | @since 0.1.0.0
      Lift,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Show
    )
  deriving anyclass
    ( -- | @since 0.1.0.0
      NFData
    )

-- | Bidirectional pattern synonym for 'LRInterval'. Construction fails when
-- the value is not within the range.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> MkLRInterval @10 @15 12
-- UnsafeLRInterval 12
--
-- @since 0.1.0.0
pattern MkLRInterval ::
  (KnownNat l, KnownNat r, Num a, Ord a, Show a) =>
  a ->
  LRInterval l r a
pattern MkLRInterval x <-
  UnsafeLRInterval x
  where
    MkLRInterval x = unsafeLRInterval x

{-# COMPLETE MkLRInterval #-}

-- | Unwraps an 'LRInterval'.
--
-- @since 0.1.0.0
unLRInterval :: LRInterval l r a -> a
unLRInterval (UnsafeLRInterval x) = x

-- | Template haskell for creating an 'LRInterval' at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkLRIntervalTH @0 @100 7)
-- UnsafeLRInterval 7
--
-- @since 0.1.0.0
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
-- @since 0.1.0.0
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

-- | Variant of 'mkLRInterval' that throws an error when given a value out of
-- bounds.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> unsafeLRInterval @0 @10 7
-- UnsafeLRInterval 7
--
-- @since 0.1.0.0
unsafeLRInterval ::
  forall l r a.
  (HasCallStack, KnownNat l, KnownNat r, Num a, Ord a, Show a) =>
  a ->
  LRInterval l r a
unsafeLRInterval x = Maybe.fromMaybe (error msg) $ mkLRInterval x
  where
    msg = lrErrMsg @l @r x "unsafeLRInterval"

-- | This function is an alias for the unchecked constructor @UnsafeLRInterval@
-- i.e. it allows us to construct a 'LRInterval' __without__ checking the
-- invariant. This is intended only for when we absolutely know the invariant
-- holds and a branch (i.e. 'unsafeLRInterval') is undesirable for performance
-- reasons. Exercise extreme caution.
--
-- @since 0.1.0.0
reallyUnsafeLRInterval :: a -> LRInterval l r a
reallyUnsafeLRInterval = UnsafeLRInterval

-- | Represents a closed interval that is left-bounded i.e.
-- @LInterval \@l x@ represents \( x \in [l, \infty) \).
--
-- @since 0.1.0.0
type LInterval :: Nat -> Type -> Type
newtype LInterval l a = UnsafeLInterval a
  deriving stock
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Generic,
      -- | @since 0.1.0.0
      Lift,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Show
    )
  deriving anyclass
    ( -- | @since 0.1.0.0
      NFData
    )

-- | Unidirectional pattern synonym for 'LInterval'. Construction fails when
-- the value is not within the range.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> MkLInterval @10 12
-- UnsafeLInterval 12
--
-- @since 0.1.0.0
pattern MkLInterval ::
  (KnownNat l, Num a, Ord a, Show a) =>
  a ->
  LInterval l a
pattern MkLInterval x <-
  UnsafeLInterval x
  where
    MkLInterval x = unsafeLInterval x

{-# COMPLETE MkLInterval #-}

-- | Unwraps an 'LInterval'.
--
-- @since 0.1.0.0
unLInterval :: LInterval l a -> a
unLInterval (UnsafeLInterval x) = x

-- | Template haskell for creating a 'LInterval' at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkLIntervalTH @0 7)
-- UnsafeLInterval 7
--
-- @since 0.1.0.0
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
-- @since 0.1.0.0
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

-- | Variant of 'mkLInterval' that throws an error when given a value out of bounds.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> unsafeLInterval @0 7
-- UnsafeLInterval 7
--
-- @since 0.1.0.0
unsafeLInterval ::
  forall l a.
  (HasCallStack, KnownNat l, Num a, Ord a, Show a) =>
  a ->
  LInterval l a
unsafeLInterval x = Maybe.fromMaybe (error msg) $ mkLInterval x
  where
    msg = lErrMsg @l x "unsafeLInterval"

-- | This function is an alias for the unchecked constructor @UnsafeLInterval@
-- i.e. it allows us to construct a 'LInterval' __without__ checking the
-- invariant. This is intended only for when we absolutely know the invariant
-- holds and a branch (i.e. 'unsafeLInterval') is undesirable for performance
-- reasons. Exercise extreme caution.
--
-- @since 0.1.0.0
reallyUnsafeLInterval :: a -> LInterval l a
reallyUnsafeLInterval = UnsafeLInterval

-- | Represents a closed interval that is right-bounded i.e.
-- @RInterval \@r x@ represents \( x \in (-\infty, r] \).
--
-- @since 0.1.0.0
type RInterval :: Nat -> Type -> Type
newtype RInterval r a = UnsafeRInterval a
  deriving stock
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Generic,
      -- | @since 0.1.0.0
      Lift,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Show
    )
  deriving anyclass
    ( -- | @since 0.1.0.0
      NFData
    )

-- | Unidirectional pattern synonym for 'RInterval'.  Construction fails when
-- the value is not within the range.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> MkRInterval @10 5
-- UnsafeRInterval 5
--
-- @since 0.1.0.0
pattern MkRInterval ::
  (KnownNat r, Num a, Ord a, Show a) =>
  a ->
  RInterval r a
pattern MkRInterval x <-
  UnsafeRInterval x
  where
    MkRInterval x = unsafeRInterval x

{-# COMPLETE MkRInterval #-}

-- | Unwraps an 'RInterval'.
--
-- @since 0.1.0.0
unRInterval :: RInterval r a -> a
unRInterval (UnsafeRInterval x) = x

-- | Template haskell for creating an 'RInterval' at compile-time.
--
-- ==== __Examples__
-- >>> $$(mkRIntervalTH @100 7)
-- UnsafeRInterval 7
--
-- @since 0.1.0.0
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
-- @since 0.1.0.0
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

-- | Variant of 'mkRInterval' that throws an error when given a value out of bounds.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> unsafeRInterval @10 7
-- UnsafeRInterval 7
--
-- @since 0.1.0.0
unsafeRInterval ::
  forall r a.
  (HasCallStack, KnownNat r, Num a, Ord a, Show a) =>
  a ->
  RInterval r a
unsafeRInterval x = Maybe.fromMaybe (error msg) $ mkRInterval x
  where
    msg = rErrMsg @r x "unsafeRInterval"

-- | This function is an alias for the unchecked constructor @UnsafeRInterval@
-- i.e. it allows us to construct a 'RInterval' __without__ checking the
-- invariant. This is intended only for when we absolutely know the invariant
-- holds and a branch (i.e. 'unsafeRInterval') is undesirable for performance
-- reasons. Exercise extreme caution.
--
-- @since 0.1.0.0
reallyUnsafeRInterval :: a -> RInterval r a
reallyUnsafeRInterval = UnsafeRInterval

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
