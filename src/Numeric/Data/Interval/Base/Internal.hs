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
module Numeric.Data.Interval.Base.Internal
  ( -- * Types
    IntervalBound (..),
    Interval (MkInterval, UnsafeInterval),

    -- * Creation
    mkInterval,
    unsafeInterval,

    -- * Singletons
    SIntervalBound (..),

    -- * Misc
    errMsg,
  )
where

import Control.DeepSeq (NFData)
import Data.Kind (Type)
import Data.Maybe qualified as Maybe
import Data.Proxy (Proxy (Proxy))
import Data.Singletons as X
  ( Sing,
    SingI (sing),
    SingKind (Demote, fromSing, toSing),
    SomeSing (SomeSing),
  )
import Data.Text qualified as T
import Data.Text.Display (Display (displayBuilder))
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as TLB
import GHC.Generics (Generic)
import GHC.Records (HasField (getField))
import GHC.Show (showSpace)
import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat, Nat, SomeNat (SomeNat), natVal, someNatVal)
import Language.Haskell.TH.Syntax (Lift)
import Numeric.Algebra.MetricSpace (MetricSpace (diffR))
import Numeric.Convert.Integer (FromInteger (fromZ), ToInteger (toZ))
import Numeric.Convert.Rational (FromRational (fromQ), ToRational (toQ))
import Numeric.Convert.Real (FromReal (fromR), ToReal (toR))
import Numeric.Data.Internal.Utils qualified as Utils
import Optics.Core (A_Getter, LabelOptic (labelOptic), to)

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

displayIntervalBounds :: IntervalBound -> IntervalBound -> Builder
displayIntervalBounds l r =
  mconcat
    [ bracketL l,
      valL l,
      ", ",
      valR r,
      bracketR r
    ]
  where
    valL (Open n) = displayShow n
    valL (Closed n) = displayShow n
    valL None = "-\8734"

    valR (Open n) = displayShow n
    valR (Closed n) = displayShow n
    valR None = "\8734"

    bracketL (Closed _) = "["
    bracketL _ = "("

    bracketR (Closed _) = "]"
    bracketR _ = ")"

    displayShow = displayBuilder . show

type SIntervalBound :: IntervalBound -> Type

-- | Singleton for 'IntervalBound'.
--
-- @since 0.1
data SIntervalBound (i :: IntervalBound) where
  SOpen :: forall (n :: Nat). (KnownNat n) => SIntervalBound (Open n)
  SClosed :: forall (n :: Nat). (KnownNat n) => SIntervalBound (Closed n)
  SNone :: SIntervalBound None

deriving stock instance Show (SIntervalBound d)

type instance Sing = SIntervalBound

instance (KnownNat k) => SingI (Open k) where
  sing = SOpen @k

instance (KnownNat k) => SingI (Closed k) where
  sing = SClosed @k

instance SingI None where
  sing = SNone

instance SingKind IntervalBound where
  type Demote IntervalBound = IntervalBound

  fromSing (SOpen @k) = Open (natVal @k Proxy)
  fromSing (SClosed @k) = Closed (natVal @k Proxy)
  fromSing SNone = None

  toSing (Open k) =
    case someNatVal k of
      SomeNat @n _ -> SomeSing (SOpen @n)
  toSing (Closed k) =
    case someNatVal k of
      SomeNat @n _ -> SomeSing (SClosed @n)
  toSing None = SomeSing SNone

-- | Represents an interval. Can be (open|closed) bounded (left|right).
--
-- ==== __Examples__
--
-- >>> import Data.Text.Display (display)
-- >>> import Data.Text qualified as T
-- >>> let x = unsafeInterval @(Open 10) @(Closed 100) 50
-- >>> putStrLn $ T.unpack $ display x
-- 50 ∈ (10, 100]
--
-- >>> let y = unsafeInterval @None @None (-2)
-- >>> putStrLn $ T.unpack $ display y
-- -2 ∈ (-∞, ∞)
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
      Ord
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance HasField "unInterval" (Interval l r a) a where
  getField (UnsafeInterval x) = x

-- | @since 0.1
instance
  ( k ~ A_Getter,
    a ~ n,
    b ~ n
  ) =>
  LabelOptic "unInterval" k (Interval l r a) (Interval l r a) a b
  where
  labelOptic = to (\(UnsafeInterval x) -> x)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  ( Show a,
    SingI l,
    SingI r
  ) =>
  Show (Interval l r a)
  where
  showsPrec i (UnsafeInterval x) =
    showParen
      (i >= 11)
      ( showString "UnsafeInterval "
          . showsPrec 11 left
          . showSpace
          . showsPrec 11 right
          . showSpace
          . showsPrec 11 x
      )
    where
      (left, right) = getInterval @l @r

-- | @since 0.1
instance
  ( Show a,
    SingI l,
    SingI r
  ) =>
  Display (Interval l r a)
  where
  displayBuilder (UnsafeInterval x) =
    mconcat
      [ displayBuilder $ show x,
        " ∈ ",
        displayIntervalBounds left right
      ]
    where
      (left, right) = getInterval @l @r

-- | @since 0.1
instance (Real a) => MetricSpace (Interval l r a) where
  diffR (UnsafeInterval x) (UnsafeInterval y) = Utils.safeDiff x y
  {-# INLINEABLE diffR #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance
  ( Num a,
    Ord a,
    SingI l,
    SingI r,
    Show a
  ) =>
  FromInteger (Interval l r a)
  where
  fromZ = unsafeInterval . fromInteger
  {-# INLINEABLE fromZ #-}

-- | @since 0.1
instance (Integral a) => ToInteger (Interval l r a) where
  toZ (UnsafeInterval x) = toInteger x
  {-# INLINEABLE toZ #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance
  ( Fractional a,
    Ord a,
    SingI l,
    SingI r,
    Show a
  ) =>
  FromRational (Interval l r a)
  where
  fromQ = unsafeInterval . fromRational
  {-# INLINEABLE fromQ #-}

-- | @since 0.1
instance (Real a) => ToRational (Interval l r a) where
  toQ (UnsafeInterval x) = toRational x
  {-# INLINEABLE toQ #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance
  ( Fractional a,
    Ord a,
    SingI l,
    SingI r,
    Show a
  ) =>
  FromReal (Interval l r a)
  where
  fromR = unsafeInterval . realToFrac
  {-# INLINEABLE fromR #-}

-- | @since 0.1
instance (Real a) => ToReal (Interval l r a) where
  toR (UnsafeInterval x) = realToFrac x
  {-# INLINEABLE toR #-}

pattern MkInterval :: a -> Interval l r a
pattern MkInterval x <- UnsafeInterval x

{-# COMPLETE MkInterval #-}

-- | Smart constructor for 'Interval'. Returns 'Nothing' if the given value
-- is not within the bounds. Note that we do not check that the bounds fit
-- within the type itself (e.g. consider @Interval @None @(Closed 200) Int8@).
--
-- ==== __Examples__
-- >>> mkInterval @(Open 10) @(Closed 100) 50
-- Just (UnsafeInterval (Open 10) (Closed 100) 50)
--
-- >>> mkInterval @(Open 10) @(Closed 100) 100
-- Just (UnsafeInterval (Open 10) (Closed 100) 100)
--
-- >>> mkInterval @(Open 10) @(Closed 100) 10
-- Nothing
--
-- >>> mkInterval @(Open 10) @(Closed 100) 101
-- Nothing
--
-- @since 0.1
mkInterval ::
  forall (l :: IntervalBound) (r :: IntervalBound) a.
  ( Num a,
    Ord a,
    SingI l,
    SingI r
  ) =>
  a ->
  Maybe (Interval l r a)
mkInterval x
  | boundedLeft && boundedRight = Just (UnsafeInterval x)
  | otherwise = Nothing
  where
    boundedLeft :: Bool
    boundedLeft = case sing @l of
      SNone -> True
      (SOpen @k) ->
        let l' = natVal @k Proxy
         in x > fromIntegral l'
      (SClosed @k) ->
        let l' = natVal @k Proxy
         in x >= fromIntegral l'

    boundedRight :: Bool
    boundedRight = case sing @r of
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
-- UnsafeInterval (Open 10) (Closed 100) 50
--
-- @since 0.1
unsafeInterval ::
  forall l r a.
  ( HasCallStack,
    Num a,
    Ord a,
    SingI l,
    SingI r,
    Show a
  ) =>
  a ->
  Interval l r a
unsafeInterval x = Maybe.fromMaybe (error msg) $ mkInterval x
  where
    msg = errMsg @l @r x "unsafeInterval"
{-# INLINEABLE unsafeInterval #-}

-- | @since 0.1
errMsg ::
  forall (l :: IntervalBound) (r :: IntervalBound) a.
  ( Show a,
    SingI l,
    SingI r
  ) =>
  a ->
  Builder ->
  String
errMsg x fnName =
  T.unpack $
    TL.toStrict $
      TLB.toLazyText msg
  where
    intervalStr = displayIntervalBounds left right
    (left, right) = getInterval @l @r
    msg =
      mconcat
        [ "Numeric.Data.Interval.Base.",
          fnName,
          ": Wanted value in ",
          intervalStr,
          ", received: ",
          displayBuilder $ show x
        ]

getInterval ::
  forall (l :: IntervalBound) (r :: IntervalBound).
  ( SingI l,
    SingI r
  ) =>
  (IntervalBound, IntervalBound)
getInterval = (fromSingleton left, fromSingleton right)
  where
    left = sing @l
    right = sing @r

fromSingleton :: SIntervalBound i -> IntervalBound
fromSingleton SNone = None
fromSingleton (SOpen @n) = Open (natVal @n Proxy)
fromSingleton (SClosed @n) = Closed (natVal @n Proxy)
