module Main (main) where

import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import Test.DocTest qualified as DT

main :: IO ()
main =
  guardOrElse'
    "RUN_DOCTEST"
    ExpectEnvSet
    (DT.doctest args)
    (putStrLn "*** Doctests Disabled ***")
  where
    args = files <> exts

files :: [String]
files =
  [ "-isrc",
    "src/Numeric/Data/Fraction.hs",
    "src/Numeric/Data/Fraction/Internal.hs",
    "src/Numeric/Data/Interval.hs",
    "src/Numeric/Data/Interval/Internal.hs",
    "src/Numeric/Data/ModN.hs",
    "src/Numeric/Data/ModN/Internal.hs",
    "src/Numeric/Data/ModP.hs",
    "src/Numeric/Data/ModP/Internal.hs",
    "src/Numeric/Data/NonNegative.hs",
    "src/Numeric/Data/NonNegative/Internal.hs",
    "src/Numeric/Data/NonZero.hs",
    "src/Numeric/Data/NonZero/Internal.hs",
    "src/Numeric/Data/Positive.hs",
    "src/Numeric/Data/Positive/Internal.hs"
  ]

exts :: [String]
exts =
  [ "-XNoStarIsType",
    "-XBangPatterns",
    "-XDataKinds",
    "-XDeriveAnyClass",
    "-XDeriveDataTypeable",
    "-XDeriveGeneric",
    "-XDeriveLift",
    "-XDerivingStrategies",
    "-XDerivingVia",
    "-XFlexibleContexts",
    "-XFlexibleInstances",
    "-XGeneralizedNewtypeDeriving",
    "-XImportQualifiedPost",
    "-XMultiParamTypeClasses",
    "-XMultiWayIf",
    "-XNegativeLiterals",
    "-XNumericUnderscores",
    "-XOverloadedStrings",
    "-XPatternSynonyms",
    "-XScopedTypeVariables",
    "-XStandaloneKindSignatures",
    "-XTypeApplications",
    "-XTypeFamilies"
  ]
