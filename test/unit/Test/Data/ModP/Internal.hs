module Test.Data.ModP.Internal (props) where

import Gens qualified
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import Numeric.Data.ModP.Internal.Primality
  ( Bezout (MkBezout),
    MaybePrime (Composite, ProbablyPrime),
    Modulus (MkModulus),
    R (R'),
    S (S'),
    T (T'),
  )
import Numeric.Data.ModP.Internal.Primality qualified as ModPI
import Test.Prelude
import Test.TestBounds (TestBounds (maxVal))

props :: TestTree
props =
  testGroup
    "Numeric.Data.ModP.Internal"
    [ isPrimeFalse,
      isPrimeTrue,
      findInverse,
      findBezout
    ]

isPrimeFalse :: TestTree
isPrimeFalse =
  testPropertyCompat "isPrime returns Composite" "isPrimeFalse" $
    property $ do
      x <- forAll composite
      Composite === ModPI.isPrime x

isPrimeTrue :: TestTree
isPrimeTrue =
  testPropertyCompat "isPrime returns ProbablyPrime" "isPrimeTrue" $
    property $ do
      p <- forAll prime
      ProbablyPrime === ModPI.isPrime p

findInverse :: TestTree
findInverse =
  testPropertyCompat "x * findInverse x m == 1 (mod m)" "findInverse" $
    property $ do
      (x, m) <- forAll coprime
      let d = ModPI.findInverse x (MkModulus m)
      annotateShow d
      x * d `mod` m === 1

findBezout :: TestTree
findBezout =
  testPropertyCompat "findBezout satisfies ax + by = (a, b)" "findBezout" $
    property $ do
      x <- forAll Gens.integer
      y <- forAll Gens.integer
      let (MkBezout (R' r) (S' s) (T' t)) = ModPI.findBezout x (MkModulus y)
      x * t + y * s === r
      gcd x y === r

coprime :: Gen (Integer, Integer)
coprime = do
  x <- HG.integral $ HR.exponential 1 maxVal
  mult <- HG.integral $ HR.exponential 1 maxVal
  let m = x * mult + 1
  pure (x, m)

composite :: Gen Integer
composite = do
  x <- HG.integral $ HR.exponentialFrom 2 2 maxVal
  pure (x + x)

-- Testing with Int to make sure our we work with bounded integral types.
-- To make this work, we internally convert to integer and do our modular
-- arithmetic there.
prime :: Gen Integer
prime = do
  x <- HG.integral $ HR.exponentialFrom 0 0 1000
  let p = primeStream !! x
  pure p

-- Infinite stream of primes
primeStream :: (Integral a) => [a]
primeStream = 2 : 3 : 5 : primes'
  where
    isPrime' (p : ps) n = p * p > n || n `rem` p /= 0 && isPrime' ps n
    isPrime' [] _ = False
    primes' = 7 : filter (isPrime' primes') (scanl (+) 11 $ cycle [2, 4, 2, 4, 6, 2, 6, 4])
