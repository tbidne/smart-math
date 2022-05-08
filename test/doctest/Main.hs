module Main (main) where

import System.Environment qualified as Env
import Test.DocTest qualified as DT

main :: IO ()
main = do
  shouldRun <- Env.lookupEnv "RUN_DOCTEST"
  case shouldRun of
    Just "true" -> DT.doctest args
    _ -> putStrLn "*** Doctests Disabled ***"
  where
    args = files <> exts

files :: [String]
files =
  [ "-isrc",
    "src/Numeric/Data/Fraction.hs",
    "src/Numeric/Data/Interval.hs",
    "src/Numeric/Data/ModN.hs",
    "src/Numeric/Data/ModP.hs",
    "src/Numeric/Data/ModP/Internal.hs",
    "src/Numeric/Data/NonNegative.hs",
    "src/Numeric/Data/Positive.hs"
  ]

exts :: [String]
exts =
  [ "-XNoStarIsType",
    "-XBangPatterns",
    "-XDataKinds",
    "-XDeriveDataTypeable",
    "-XDeriveGeneric",
    "-XDeriveLift",
    "-XDerivingStrategies",
    "-XDerivingVia",
    "-XFlexibleContexts",
    "-XFlexibleInstances",
    "-XGeneralizedNewtypeDeriving",
    "-XImportQualifiedPost",
    "-XNegativeLiterals",
    "-XNumericUnderscores",
    "-XPatternSynonyms",
    "-XScopedTypeVariables",
    "-XStandaloneKindSignatures",
    "-XTypeApplications",
    "-XTypeFamilies"
  ]
