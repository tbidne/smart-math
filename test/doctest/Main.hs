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
    "src/Numeric/Data/Fraction/Algebra.hs",
    "src/Numeric/Data/Fraction/Algebra/Internal.hs",
    "src/Numeric/Data/Fraction/Base.hs",
    "src/Numeric/Data/Fraction/Base/Internal.hs",
    "src/Numeric/Data/Interval/Algebra.hs",
    "src/Numeric/Data/Interval/Algebra/Internal.hs",
    "src/Numeric/Data/Interval/Base.hs",
    "src/Numeric/Data/Interval/Base/Internal.hs",
    "src/Numeric/Data/ModN/Algebra.hs",
    "src/Numeric/Data/ModN/Algebra/Internal.hs",
    "src/Numeric/Data/ModN/Base.hs",
    "src/Numeric/Data/ModN/Base/Internal.hs",
    "src/Numeric/Data/ModP/Algebra.hs",
    "src/Numeric/Data/ModP/Algebra/Internal.hs",
    "src/Numeric/Data/ModP/Base.hs",
    "src/Numeric/Data/ModP/Base/Internal.hs",
    "src/Numeric/Data/NonNegative/Algebra.hs",
    "src/Numeric/Data/NonNegative/Algebra/Internal.hs",
    "src/Numeric/Data/NonNegative/Base.hs",
    "src/Numeric/Data/NonNegative/Base/Internal.hs",
    "src/Numeric/Data/NonZero/Algebra.hs",
    "src/Numeric/Data/NonZero/Algebra/Internal.hs",
    "src/Numeric/Data/NonZero/Base.hs",
    "src/Numeric/Data/NonZero/Base/Internal.hs",
    "src/Numeric/Data/Positive/Algebra.hs",
    "src/Numeric/Data/Positive/Algebra/Internal.hs",
    "src/Numeric/Data/Positive/Base.hs",
    "src/Numeric/Data/Positive/Base/Internal.hs"
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
    "-XLambdaCase",
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
