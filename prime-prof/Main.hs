module Main (main) where

import Numeric.Data.ModP.Internal qualified as ModPI

main :: IO ()
main = print x
  where
    x = ModPI.isPrime 100_003