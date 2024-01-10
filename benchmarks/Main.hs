module Main (main) where

import Data.List qualified as L
import Numeric.Data.ModP.Internal.Primality qualified as Primality
import Test.Tasty.Bench
  ( Benchmark,
    Benchmarkable,
    bench,
    bgroup,
    defaultMain,
    nf,
  )

main :: IO ()
main = do
  defaultMain
    [ bgroup "isPrime" isPrimeBenchmarks
    ]

isPrimeBenchmarks :: [Benchmark]
isPrimeBenchmarks =
  [ isPrimeN 2,
    isPrimeN 11,
    isPrimeN 1009,
    isPrimeN 10_007,
    isPrimeN 100_003,
    isPrimeN 1_000_003
  ]

isPrimeN :: Integer -> Benchmark
isPrimeN n =
  bgroup
    (showN n)
    [ bench "default" (benchIsPrimeDefault n),
      bench "arithmoi" (benchIsPrimeArithmoi n)
    ]

benchIsPrimeDefault :: Integer -> Benchmarkable
benchIsPrimeDefault = nf Primality.isPrimeDefault

benchIsPrimeArithmoi :: Integer -> Benchmarkable
benchIsPrimeArithmoi = nf Primality.isPrimeArithmoi

-- | e.g. showN 1_000_000 -> "1_000_000"
showN :: Integer -> String
showN = reverse . L.intercalate "_" . group3 . reverse . show
  where
    group3 [] = []
    group3 [a] = [[a]]
    group3 [a, b] = [[a, b]]
    group3 (a : b : c : ds) = [a, b, c] : group3 ds
