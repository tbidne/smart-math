module Main (main) where

import Numeric.Data.ModP.Internal.Primality qualified as ModPI
import System.Environment (getArgs)
import Text.Read qualified as TR

main :: IO ()
main = getN >>= print . ModPI.isPrime

getN :: IO Integer
getN = do
  getArgs >>= \case
    [] -> pure 100_003
    [x] -> case TR.readMaybe x of
      Nothing -> error $ "Could not read integer: " ++ show x
      Just n -> pure n
    other ->
      error $ "Expected empty or a single integer, received: " ++ show other
