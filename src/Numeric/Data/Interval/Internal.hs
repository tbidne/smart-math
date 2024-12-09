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
module Numeric.Data.Interval.Internal
  ( -- * Types
    IntervalBound (..),
    Interval (MkInterval, UnsafeInterval),

    -- * Creation
    mkInterval,
    unsafeInterval,

    -- * Singletons
    SIntervalBound (..),
    SingIntervalBound (..),
    withSingIntervalBound,

    -- * Misc
    errMsg,
  )
where

import Control.DeepSeq (NFData)
import Data.Kind (Constraint, Type)
import Data.Maybe qualified as Maybe
import Data.Proxy (Proxy (Proxy))
import Data.Text qualified as T
import Data.Text.Display (Display (displayBuilder))
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as TLB
import GHC.Generics (Generic)
import GHC.Records (HasField (getField))
import GHC.Show (showSpace)
import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat, Nat, natVal)
import Language.Haskell.TH.Syntax (Lift)
import Numeric.Algebra.MetricSpace (MetricSpace (diffR))
import Numeric.Data.Internal.Utils qualified as Utils
import Numeric.Literal.Integer (FromInteger (fromZ), ToInteger (toZ))
import Numeric.Literal.Rational (FromRational (fromQ), ToRational (toQ))
import Numeric.Literal.Real (FromReal (fromR), ToReal (toR))
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
    SingIntervalBound l,
    SingIntervalBound r
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
    SingIntervalBound l,
    SingIntervalBound r
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
    SingIntervalBound l,
    SingIntervalBound r,
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
    SingIntervalBound l,
    SingIntervalBound r,
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
    SingIntervalBound l,
    SingIntervalBound r,
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
-- UnsafeInterval (Open 10) (Closed 100) 50
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

-- | @since 0.1
errMsg ::
  forall l r a.
  ( Show a,
    SingIntervalBound l,
    SingIntervalBound r
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
        [ "Numeric.Data.Interval.",
          fnName,
          ": Wanted value in ",
          intervalStr,
          ", received: ",
          displayBuilder $ show x
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
