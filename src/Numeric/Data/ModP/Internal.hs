{-# OPTIONS_GHC -Wno-identities #-}

-- See the note on Modulus for why this warning is disabled

-- | Internal tools for modular arithmetic and primality testing. The main
-- functions are 'isPrime' and 'findInverse', though others are exported for
-- testing.
--
-- @since 0.1
module Numeric.Data.ModP.Internal
  ( -- * Type
    ModP (MkModP, ..),

    -- ** Functions
    mkModP,
    unsafeModP,
    invert,
    reallyUnsafeModP,

    -- ** Misc
    errMsg,

    -- * Primality Testing
    MaybePrime (..),
    isPrime,
    isPrimeTrials,
    millerRabin,

    -- ** Helper Types
    -- $primality-helper
    Modulus (..),
    Pow (..),
    Mult (..),
    Rand (..),

    -- ** Helper Functions
    trial,
    isWitness,
    sqProgression,
    factor2,

    -- * Multiplicative Inverses
    Bezout (..),
    R (..),
    S (..),
    T (..),
    findInverse,
    findBezout,
  )
where

import Control.DeepSeq (NFData)
import Data.Bounds (AnyLowerBounded, AnyUpperBounded, LowerBounded, UpperBounded)
import Data.Data (Proxy (Proxy))
import Data.Kind (Type)
import Data.Text.Display (Display (displayBuilder))
import Data.Typeable (Typeable)
import Data.Word (Word16)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat, Nat, natVal)
import Language.Haskell.TH.Syntax (Lift)
import Numeric.Algebra.Additive.AGroup (AGroup ((.-.)))
import Numeric.Algebra.Additive.AMonoid (AMonoid (zero))
import Numeric.Algebra.Additive.ASemigroup (ASemigroup ((.+.)))
import Numeric.Algebra.Field (Field)
import Numeric.Algebra.Multiplicative.MGroup (MGroup ((.%.)))
import Numeric.Algebra.Multiplicative.MMonoid (MMonoid (one))
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup ((.*.)))
import Numeric.Algebra.Ring (Ring)
import Numeric.Algebra.Semifield (Semifield)
import Numeric.Algebra.Semiring (Semiring)
import Numeric.Data.Internal.Utils qualified as Utils
import Numeric.Literal.Integer (FromInteger (afromInteger))
import System.Random (UniformRange)
import System.Random qualified as Rand
import System.Random.Stateful qualified as RandState

-- $setup
-- >>> import Data.Int (Int8)

-- see NOTE: [Safe finite modular rounding]

-- | Newtype wrapper that represents \( \mathbb{Z}/p\mathbb{Z} \) for prime @p@.
-- 'ModP' is a 'Numeric.Algebra.Field.Field' i.e. supports addition,
-- subtraction, multiplication, and division.
--
-- When constructing a @'ModP' p a@ we must verify that @p@ is prime and the
-- type @a@ is large enough to accommodate @p@, hence the possible failure.
--
-- ==== __Examples__
--
-- >>> import Data.Text.Display (display)
-- >>> display $ unsafeModP @7 10
-- "3 (mod 7)"
--
-- @since 0.1
type ModP :: Nat -> Type -> Type
newtype ModP p a = UnsafeModP a
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
      LowerBounded,
      -- | @since 0.1
      NFData,
      -- | @since 0.1
      UpperBounded
    )

-- | Unidirectional pattern synonym for 'ModP'. This allows us to pattern
-- match on a modp term without exposing the unsafe internal details.
--
-- @since 0.1
pattern MkModP :: a -> ModP p a
pattern MkModP x <- UnsafeModP x

{-# COMPLETE MkModP #-}

-- | @since 0.1
instance (KnownNat p, Show a) => Show (ModP p a) where
  -- manual so we show "MkModP" instead of "UnsafeModP"
  showsPrec i (UnsafeModP x) =
    showParen
      (i >= 11)
      (showString "MkModP " . showsPrec 11 x . showString modStr)
    where
      modStr = " (mod " <> show p' <> ")"
      p' = natVal @p Proxy
  {-# INLINEABLE showsPrec #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance
  ( AnyUpperBounded a,
    Integral a,
    KnownNat p,
    Typeable a
  ) =>
  Bounded (ModP p a)
  where
  minBound = unsafeModP 0
  maxBound = unsafeModP $ fromIntegral (natVal @p Proxy - 1)
  {-# INLINEABLE minBound #-}
  {-# INLINEABLE maxBound #-}

-- | @since 0.1
instance (KnownNat p, Show a) => Display (ModP p a) where
  displayBuilder (UnsafeModP x) =
    mconcat
      [ displayBuilder $ show x,
        displayBuilder @String " (mod ",
        displayBuilder $ show p',
        displayBuilder @String ")"
      ]
    where
      p' = natVal @p Proxy

-- | @since 0.1
instance
  ( AnyUpperBounded a,
    Integral a,
    KnownNat p
  ) =>
  ASemigroup (ModP p a)
  where
  UnsafeModP x .+. UnsafeModP y =
    UnsafeModP $ Utils.modSafeAdd x y (fromIntegral p')
    where
      p' = natVal @p Proxy
  {-# INLINEABLE (.+.) #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance
  ( AnyUpperBounded a,
    Integral a,
    KnownNat p,
    Typeable a
  ) =>
  AMonoid (ModP p a)
  where
  zero = unsafeModP 0
  {-# INLINEABLE zero #-}

-- | @since 0.1
instance
  ( AnyLowerBounded a,
    AnyUpperBounded a,
    Integral a,
    KnownNat p,
    Typeable a
  ) =>
  AGroup (ModP p a)
  where
  UnsafeModP x .-. UnsafeModP y =
    UnsafeModP $ Utils.modSafeSub x y (fromIntegral p')
    where
      p' = natVal @p Proxy
  {-# INLINEABLE (.-.) #-}

-- | @since 0.1
instance
  ( AnyUpperBounded a,
    Integral a,
    KnownNat p
  ) =>
  MSemigroup (ModP p a)
  where
  UnsafeModP x .*. UnsafeModP y =
    UnsafeModP $ Utils.modSafeMult x y (fromIntegral p')
    where
      p' = natVal @p Proxy
  {-# INLINEABLE (.*.) #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance
  ( AnyUpperBounded a,
    Integral a,
    KnownNat p,
    Typeable a
  ) =>
  MMonoid (ModP p a)
  where
  one = unsafeModP 1
  {-# INLINEABLE one #-}

-- | @since 0.1
instance
  ( AnyUpperBounded a,
    Integral a,
    KnownNat p,
    Typeable a
  ) =>
  MGroup (ModP p a)
  where
  x .%. d = x .*. invert d
  {-# INLINEABLE (.%.) #-}

-- | @since 0.1
instance
  ( AnyUpperBounded a,
    Integral a,
    KnownNat p,
    Typeable a
  ) =>
  Semiring (ModP p a)

-- | @since 0.1
instance
  ( AnyLowerBounded a,
    AnyUpperBounded a,
    Integral a,
    KnownNat p,
    Typeable a
  ) =>
  Ring (ModP p a)

-- | @since 0.1
instance
  ( AnyUpperBounded a,
    Integral a,
    KnownNat p,
    Typeable a
  ) =>
  Semifield (ModP p a)

-- | @since 0.1
instance
  ( AnyLowerBounded a,
    AnyUpperBounded a,
    Integral a,
    KnownNat p,
    Typeable a
  ) =>
  Field (ModP p a)

-- | __WARNING: Partial__
--
-- @since 0.1
instance
  ( AnyUpperBounded a,
    Integral a,
    KnownNat p,
    Typeable a
  ) =>
  FromInteger (ModP p a)
  where
  afromInteger = unsafeModP . fromInteger
  {-# INLINEABLE afromInteger #-}

-- | Constructor for 'ModP'. Fails if @p@ is not prime. This uses the
-- Miller-Rabin primality test, which has complexity \(O(k \log^3 p)\), and we
-- take \(k = 100\). See
-- [wikipedia](https://en.wikipedia.org/wiki/Miller-Rabin_primality_test#Complexity)
-- for more details.
--
-- ==== __Examples__
-- >>> mkModP @5 7
-- Right (MkModP 2 (mod 5))
--
-- >>> mkModP @10 7
-- Left "Received non-prime: 10"
--
-- >>> mkModP @128 (9 :: Int8)
-- Left "Type 'Int8' has a maximum size of 127. This is not large enough to safely implement mod 128."
--
-- @since 0.1
mkModP ::
  forall p a.
  ( AnyUpperBounded a,
    Integral a,
    KnownNat p,
    Typeable a
  ) =>
  a ->
  Either String (ModP p a)
mkModP x = maybe modP Left (Utils.checkModBound x p')
  where
    modP = case isPrime p' of
      Composite -> Left $ "Received non-prime: " <> show p'
      ProbablyPrime -> Right $ UnsafeModP x'

    p' = toInteger $ natVal @p Proxy
    x' = x `mod` fromIntegral p'
{-# INLINEABLE mkModP #-}

-- | Variant of 'mkModP' that throws an error when given a non-prime.
--
-- __WARNING: Partial__
--
-- ==== __Examples__
-- >>> unsafeModP @7 12
-- MkModP 5 (mod 7)
--
-- @since 0.1
unsafeModP ::
  forall p a.
  ( AnyUpperBounded a,
    HasCallStack,
    Integral a,
    KnownNat p,
    Typeable a
  ) =>
  a ->
  ModP p a
unsafeModP x = case mkModP x of
  Right mp -> mp
  Left err -> error $ errMsg "unsafeModP" err
{-# INLINEABLE unsafeModP #-}

-- | @since 0.1
errMsg :: String -> String -> String
errMsg fn msg =
  mconcat
    [ "Numeric.Data.ModP.",
      fn,
      ": ",
      msg
    ]

-- | Given non-zero \(d\), returns the inverse i.e. finds \(e\) s.t.
--
-- \[
-- de \equiv 1 \pmod p.
-- \]
--
-- ==== __Examples__
--
-- >>> invert $ unsafeModP @7 5
-- MkModP 3 (mod 7)
--
-- >>> invert $ unsafeModP @19 12
-- MkModP 8 (mod 19)
--
-- @since 0.1
invert :: forall p a. (Integral a, KnownNat p) => ModP p a -> ModP p a
invert (UnsafeModP d) = reallyUnsafeModP $ fromIntegral $ findInverse d' p'
  where
    p' = MkModulus $ fromIntegral $ natVal @p Proxy
    d' = toInteger d
{-# INLINEABLE invert #-}

-- | This function reduces the argument modulo @p@ but does __not__ check
-- that @p@ is prime. Note that the correct behavior of some functionality
-- (e.g. division) is reliant on primality, so this is dangerous. This is
-- intended only for when we absolutely know @p@ is prime and the check
-- is undesirable for performance reasons. Exercise extreme caution.
--
-- @since 0.1
reallyUnsafeModP :: forall p a. (Integral a, KnownNat p) => a -> ModP p a
reallyUnsafeModP = UnsafeModP . (`mod` p')
  where
    p' = fromIntegral $ natVal @p Proxy
{-# INLINEABLE reallyUnsafeModP #-}

-- | Result of running Miller-Rabin algorithm. At best we can determine if
-- some @n@ is definitely composite or "probably prime".
--
-- @since 0.1
type MaybePrime :: Type
data MaybePrime
  = Composite
  | ProbablyPrime
  deriving stock (Eq, Show)

instance Semigroup MaybePrime where
  Composite <> _ = Composite
  ProbablyPrime <> r = r

instance Monoid MaybePrime where
  mempty = ProbablyPrime

-- | Tests primality via the Miller-Rabin algorithm with 100 trials. Returns
-- 'Composite' if the number is definitely composite, otherwise
-- 'ProbablyPrime'.
--
-- ==== __Examples__
-- >>> isPrime 7
-- ProbablyPrime
--
-- >>> isPrime 22
-- Composite
--
-- >>> isPrime 373
-- ProbablyPrime
--
-- @since 0.1
isPrime :: Integer -> MaybePrime
isPrime = isPrimeTrials 100
{-# INLINEABLE isPrime #-}

-- | 'isPrime' that takes in an additional 'Word16' parameter for the number
-- of trials to run. The more trials, the more confident we can be in
-- 'ProbablyPrime'.
--
-- ==== __Examples__
-- >>> isPrimeTrials 1 91
-- ProbablyPrime
--
-- >>> isPrimeTrials 2 91
-- Composite
--
-- Note: False positives can be found via:
--
-- @
-- -- search for \"ProbablyPrime\" after 1 trial in the composite sequence
-- -- for a given prime p
-- counter p = filter ((== ProbablyPrime) . snd) $
--   fmap (\x -> (x, isPrimeTrials 1 x)) [p + p, p + p + p ..]
-- @
--
-- @since 0.1
isPrimeTrials :: Word16 -> Integer -> MaybePrime
isPrimeTrials _ 1 = Composite
isPrimeTrials _ 2 = ProbablyPrime
isPrimeTrials numTrials n
  | even n = Composite
  | otherwise = millerRabin (MkModulus n) numTrials
{-# INLINEABLE isPrimeTrials #-}

-- $primality-helper
-- For the following functions/types, a core concept is rewriting our \(n\) as
--
-- \[
--   n = 2^r d + 1,
-- \]
--
-- where \(d\) is odd i.e. we have factored out 2 as much as possible.
-- We use newtypes to track these numbers.

-- | Represents a modulus. When testing for primality, this is the \(n\) in
-- \(n = 2^{r} d + 1\).
--
-- @since 0.1
type Modulus :: Type
newtype Modulus = MkModulus Integer
  deriving stock (Eq, Show)
  deriving newtype (Enum, Integral, Ord, Num, Real)

-- GHC 9+ is complaining that "Call of toInteger :: Integer -> Integer can
-- probably be omitted" when deriving Integral for all these types in this
-- module. My guess is the derived instance is generating toInteger for some
-- reason. Until we investigate further, disabling the -Widentities warning
-- is the easiest workaround.

-- | The \(r\) in \(n = 2^{r} d + 1\).
--
-- @since 0.1
type Pow :: Type
newtype Pow = MkPow Integer
  deriving stock (Eq, Show, Ord)
  deriving (Enum, Integral, Num, Real) via Integer

-- | The \(d\) in \(n = 2^{r} d + 1\).
--
-- @since 0.1
type Mult :: Type
newtype Mult = MkMult Integer
  deriving stock (Eq, Show, Ord)
  deriving (Enum, Integral, Num, Real) via Integer

-- | Randomly generated \(m \in [2, n - 2] \) for testing \(n\)'s primality.
--
-- @since 0.1
type Rand :: Type
newtype Rand = MkRand Integer
  deriving stock (Eq, Show, Ord)
  deriving (Enum, Integral, Num, Real) via Integer

-- | @since 0.1
instance UniformRange Rand where
  uniformRM (MkRand l, MkRand u) = fmap MkRand . RandState.uniformRM (l, u)
  {-# INLINEABLE uniformRM #-}

-- | Miller-Rabin algorithm. Takes in the \(n\) to be tested and the number
-- of trials to perform. The higher the trials, the higher our confidence
-- in 'ProbablyPrime'.
millerRabin :: Modulus -> Word16 -> MaybePrime
millerRabin 2 = const ProbablyPrime
millerRabin modulus@(MkModulus n) = go gen
  where
    gen = Rand.mkStdGen 373
    powMult = factor2 (modulus - 1)
    range = RandState.uniformRM (2, MkRand (n - 2))

    go _ 0 = ProbablyPrime
    go g !k =
      let (randomVal, g') = RandState.runStateGen g range
       in case trial modulus powMult randomVal of
            Composite -> Composite
            ProbablyPrime -> go g' (k - 1)
{-# INLINEABLE millerRabin #-}

-- | For \(n, r, d, x\) with \(n = 2^{r} d + 1\) and \(x \in [2, n - 2] \),
-- returns 'Composite' if \(n\) is definitely composite, 'ProbablyPrime'
-- otherwise.
--
-- ==== __Examples__
-- >>> trial 12 (factor2 (12 - 1)) 3
-- Composite
--
-- >>> trial 7 (factor2 (7 - 1)) 3
-- ProbablyPrime
--
-- @since 0.1
trial :: Modulus -> (Pow, Mult) -> Rand -> MaybePrime
trial modulus@(MkModulus n) (r, d) (MkRand a)
  -- x = 1 or n - 1 -> skip
  | x == 1 || x == n - 1 = ProbablyPrime
  -- if we found a witness then n is definitely composite
  | otherwise = isWitness modulus r (MkRand x)
  where
    x = a ^ d `mod` n
{-# INLINEABLE trial #-}

-- | For \(n, r, x\) with \(n = 2^{r} d + 1\) and some
-- \(x \equiv a^d \pmod n \), returns 'Composite' if \(x\) is a witness to
-- \(n\) being composite. Otherwise returns 'ProbablyPrime'.
--
-- ==== __Examples__
-- >>> let (pow, mult) = factor2 (12 - 1)
-- >>> let testVal = 3 ^ mult `mod` 12
-- >>> isWitness 12 pow testVal
-- Composite
--
-- >>> let (pow, mult) = factor2 (7 - 1)
-- >>> let testVal = 3 ^ mult `mod` 7
-- >>> isWitness 7 pow testVal
-- ProbablyPrime
--
-- @since 0.1
isWitness :: Modulus -> Pow -> Rand -> MaybePrime
isWitness modulus@(MkModulus n) r (MkRand x) = coprimeToResult coprime
  where
    squares = take (fromIntegral r) $ sqProgression modulus x
    coprime = (n - 1) `elem` squares
    coprimeToResult True = ProbablyPrime
    coprimeToResult False = Composite
{-# INLINEABLE isWitness #-}

-- | For \(n, x\), returns the infinite progression
--
-- \[
-- x, x^2, x^4, x^8, \ldots \pmod n.
-- \]
--
-- ==== __Examples__
-- >>> take 5 $ sqProgression 7 3
-- [3,2,4,2,4]
--
-- @since 0.1
sqProgression :: Modulus -> Integer -> [Integer]
sqProgression (MkModulus n) = go
  where
    go !y = y : go (y ^ (2 :: Int) `mod` n)
{-# INLINEABLE sqProgression #-}

-- | Given \(n\), returns \((r, d)\) such that \(n = 2^r d\) with \(d\) odd
-- i.e. \(2\) has been factored out.
--
-- ==== __Examples__
-- >>> factor2 7
-- (MkPow 0,MkMult 7)
--
-- >>> factor2 8
-- (MkPow 3,MkMult 1)
--
-- >>> factor2 20
-- (MkPow 2,MkMult 5)
--
-- @since 0.1
factor2 :: Modulus -> (Pow, Mult)
factor2 (MkModulus n) = go (MkPow 0, MkMult n)
  where
    go (!r, !d)
      | d == 2 = (r + 1, 1)
      | even d = go (r + 1, d `div` 2)
      | otherwise = (r, d)
{-# INLINEABLE factor2 #-}

-- | For \(a, p\), finds the multiplicative inverse of \(a\) in
-- \(\mathbb{Z}/p\mathbb{Z}\). That is, finds /e/ such that
--
-- \[
-- ae \equiv 1 \pmod p.
-- \]
--
-- Note: The returned \(e\) is only an inverse when \(a\) and \(p\) are
-- coprime i.e. \((a,p) = 1\). Of course this is guaranteed when \(p\) is
-- prime and \(0 < a < p \), but it otherwise not true in general.
--
-- Also, this function requires division, it is partial when
-- the modulus is 0.
--
-- @since 0.1
findInverse :: Integer -> Modulus -> Integer
findInverse a (MkModulus p) = aInv `mod` p
  where
    (MkBezout _ _ (T' aInv)) = eec p a
{-# INLINEABLE findInverse #-}

-- | @since 0.1
findBezout :: Integer -> Modulus -> Bezout
findBezout a (MkModulus p) = eec p a
{-# INLINEABLE findBezout #-}

-- | @since 0.1t
type Bezout :: Type
data Bezout = MkBezout
  { bzGcd :: !R,
    bzS :: !S,
    bzT :: !T
  }
  deriving stock (Eq, Show)

-- | @since 0.1
type R :: Type
newtype R = R' Integer
  deriving stock (Eq, Show, Ord)
  deriving (Enum, Integral, Num, Real) via Integer

-- | @since 0.1
type S :: Type
newtype S = S' Integer
  deriving stock (Eq, Show, Ord)
  deriving (Enum, Integral, Num, Real) via Integer

-- | @since 0.1
type T :: Type
newtype T = T' Integer
  deriving stock (Eq, Show, Ord)
  deriving (Enum, Integral, Num, Real) via Integer

-- Solves for Bezout's identity using the extended euclidean algorithm:
-- https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Pseudocode
eec :: Integer -> Integer -> Bezout
eec a b = go initOldR initR initOldS initS initOldT initT
  where
    (initOldR, initR) = (R' a, R' b)
    (initOldS, initS) = (S' 1, S' 0)
    (initOldT, initT) = (T' 0, T' 1)

    go oldR 0 oldS _ oldT _ = MkBezout oldR oldS oldT
    go !oldR !r !oldS !s !oldT !t =
      let oldR' = r
          oldS' = s
          oldT' = t
          (R' q, r') = oldR `quotRem` r
          s' = oldS - S' q * s
          t' = oldT - T' q * t
       in go oldR' r' oldS' s' oldT' t'
{-# INLINEABLE eec #-}
