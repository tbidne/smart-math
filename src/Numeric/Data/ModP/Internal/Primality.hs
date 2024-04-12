{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-identities #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- See the note on Modulus for why this warning is disabled

-- | Internal tools for modular arithmetic and primality testing. The main
-- functions are 'isPrime' and 'findInverse', though others are exported for
-- testing.
--
-- @since 0.1
module Numeric.Data.ModP.Internal.Primality
  ( -- * Primality Testing
    MaybePrime (..),
    isPrime,
    isPrimeTrials,
    millerRabin,

    -- ** Arithmoi vs. Default
    -- $arithmoi
    isPrimeArithmoi,
    isPrimeDefault,

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

    -- ** Arithmoi vs. Default
    invert,
    invertArithmoi,
    invertDefault,

    -- ** Types / Low-level
    Bezout (..),
    R (..),
    S (..),
    T (..),
    findInverse,
    findBezout,

    -- * Misc
    errMsg,
  )
where

import Control.DeepSeq (NFData)
import Data.Data (Proxy (Proxy))
import Data.Kind (Type)
import GHC.Generics (Generic)
import GHC.TypeNats (KnownNat, natVal)
import Numeric.Natural (Natural)
import System.Random (UniformRange)
import System.Random qualified as Rand
import System.Random.Stateful qualified as RandState

#if USE_ARITHMOI
import Data.Mod (Mod)
import Data.Mod qualified as Mod
import Math.NumberTheory.Primes.Testing qualified as AM.Primes.Testing
#endif

-- | Result of running Miller-Rabin algorithm. At best we can determine if
-- some @n@ is definitely composite or "probably prime".
--
-- @since 0.1
type MaybePrime :: Type
data MaybePrime
  = Composite
  | ProbablyPrime
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Semigroup MaybePrime where
  Composite <> _ = Composite
  ProbablyPrime <> r = r

instance Monoid MaybePrime where
  mempty = ProbablyPrime

-- TODO: Turns out, isPrime is slow. For example, isPrime 1_000_003 takes
-- quite a long time. Basic profiling (:set +s in ghci) shows that memory
-- scales with the prime. This is probably much worse than it should be.
--
-- The optional flag arithmoi enables the arithmoi package, which is much
-- faster. But it would be nice if we could improve the default as well.

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
#if USE_ARITHMOI
isPrime = isPrimeArithmoi
#else
isPrime = isPrimeDefault
#endif
{-# INLINEABLE isPrime #-}

-- $arithmoi
--
-- By default, our isPrime function implements miller-rabin directly.
-- Unfortunately, the performance scales poorly (this is an issue with our
-- implementation, not miller-rabin). Thus we provide the optional flag
-- @arithmoi@ that instead uses the arithmoi package. Arithmoi is much faster,
-- though it is not a light dependency, hence the option.
--
-- In other words, the flag controls the tradeoff between isPrime speed vs.
-- dependency footprint. So why do we also provide isPrimeDefault and
-- isPrimeArithmoi? Benchmarking. We want to benchmark the difference, hence
-- we need both available when the flag is on.

-- | Uses arithmoi if available, otherwise errors.
isPrimeArithmoi :: Integer -> MaybePrime
#if USE_ARITHMOI
isPrimeArithmoi n =
  if AM.Primes.Testing.isPrime n
    then ProbablyPrime
    else Composite
#else
isPrimeArithmoi =
  error
    $ errMsg
    "Internal.Primality.isPrimeArithmoi"
    "arithmoi flag is disabled. Either turn the flag on or use one of isPrime, isPrimeDefault."
#endif

-- | 'isPrimeTrials' with 100 trials.
isPrimeDefault :: Integer -> MaybePrime
isPrimeDefault = isPrimeTrials 100

-- | 'isPrime' that takes in an additional 'Int' parameter for the number
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
isPrimeTrials :: Int -> Integer -> MaybePrime
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
millerRabin :: Modulus -> Int -> MaybePrime
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

{- ORMOLU_DISABLE -}

-- | Finds the multiplicative inverse.
invert :: forall p. (KnownNat p) => Natural -> Natural
#if USE_ARITHMOI
invert = invertArithmoi @p
#else
invert = invertDefault @p
#endif
{-# INLINEABLE invert #-}

-- | Finds the multiplicative inverse with arithmoi if available, otherwise
-- errors.
invertArithmoi :: forall p. (KnownNat p) => Natural -> Natural
#if USE_ARITHMOI
invertArithmoi d =
  case Mod.invertMod (fromIntegral d :: Mod p) of
    Nothing ->
      error
        $ errMsg
            ".Internal.Primality.invert"
            ("Could not find inverse of " ++ (show d) ++ " (mod " ++ show p' ++ ")")
    Just n -> Mod.unMod n
  where
    p' = natVal @p Proxy
#else
invertArithmoi =
  error
    $ errMsg
    "Internal.Primality.invertArithmoi"
    "arithmoi flag is disabled. Either turn the flag on or use one of invert, invertDefault."
#endif

-- | Finds the multiplicative inverse using the built-in algorithm.
invertDefault :: forall p. (KnownNat p) => Natural -> Natural
invertDefault d = fromIntegral $ findInverse d' p'
  where
    p' = MkModulus $ fromIntegral $ natVal @p Proxy
    d' = toInteger d

{- ORMOLU_ENABLE -}

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

-- | @since 0.1
errMsg :: String -> String -> String
errMsg fn msg =
  mconcat
    [ "Numeric.Data.ModP.",
      fn,
      ": ",
      msg
    ]
