module Test.Data.ModP.Internal (props) where

import Gens qualified
import Hedgehog (MonadGen, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import MaxRuns (MaxRuns (..))
import Numeric.Data.ModP.Internal
  ( Bezout (..),
    MaybePrime (..),
    Modulus (..),
    R (..),
    S (..),
    T (..),
  )
import Numeric.Data.ModP.Internal qualified as ModPI
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.TestBounds (TestBounds (..))
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Numeric.Data.ModP.Internal"
    [ isPrimeFalse,
      isPrimeTrue,
      findInverse,
      findBezout
    ]

isPrimeFalse :: TestTree
isPrimeFalse = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "isPrime returns Composite" "isPrimeFalse" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll composite
        Composite === ModPI.isPrime x

isPrimeTrue :: TestTree
isPrimeTrue = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "isPrime returns ProbablyPrime" "isPrimeTrue" $
    H.withTests limit $
      H.property $ do
        p <- H.forAll prime
        ProbablyPrime === ModPI.isPrime p

findInverse :: TestTree
findInverse = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "x * findInverse x m == 1 (mod m)" "findInverse" $
    H.withTests limit $
      H.property $ do
        (x, m) <- H.forAll coprime
        let d = ModPI.findInverse x (MkModulus m)
        H.annotateShow d
        x * d `mod` m === 1

findBezout :: TestTree
findBezout = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "findBezout satisfies ax + by = (a, b)" "findBezout" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.integer
        y <- H.forAll Gens.integer
        let (MkBezout (R' r) (S' s) (T' t)) = ModPI.findBezout x (MkModulus y)
        x * t + y * s === r
        gcd x y === r

coprime :: MonadGen m => m (Integer, Integer)
coprime = do
  x <- HG.integral $ HR.exponential 1 maxVal
  mult <- HG.integral $ HR.exponential 1 maxVal
  let m = x * mult + 1
  pure (x, m)

composite :: MonadGen m => m Integer
composite = do
  x <- HG.integral $ HR.exponentialFrom 2 2 maxVal
  pure (x + x)

-- Testing with Int to make sure our we work with bounded integral types.
-- To make this work, we internally convert to integer and do our modular
-- arithmetic there.
prime :: MonadGen m => m Integer
prime = do
  x <- HG.integral $ HR.exponentialFrom 0 0 1000
  let p = primeStream !! x
  pure p

-- Infinite stream of primes
primeStream :: Integral a => [a]
primeStream = 2 : 3 : 5 : primes'
  where
    isPrime' (p : ps) n = p * p > n || n `rem` p /= 0 && isPrime' ps n
    isPrime' [] _ = False
    primes' = 7 : filter (isPrime' primes') (scanl (+) 11 $ cycle [2, 4, 2, 4, 6, 2, 6, 4])
