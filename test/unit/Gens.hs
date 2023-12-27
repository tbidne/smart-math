-- | Exports generators.
--
-- @since 0.1
module Gens
  ( -- * Basic

    -- ** Integral
    integer,
    natural,
    fraction,
    modN,
    modP,
    nonNegative,
    nonZero,
    positive,

    -- * NonZero

    -- ** Specializations
    integerNZ,
    fractionNonZero,
    modPNonZero,
    nonNegativeNonZero,
    positiveNonZero,
  )
where

import GHC.Natural (Natural)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as HG
import Hedgehog.Range (Range)
import Hedgehog.Range qualified as HR
import Numeric.Data.Fraction (Fraction, unsafeFraction)
import Numeric.Data.ModN (ModN, mkModN)
import Numeric.Data.ModP (ModP, reallyUnsafeModP)
import Numeric.Data.NonNegative (NonNegative, unsafeNonNegative)
import Numeric.Data.NonZero (NonZero, unsafeNonZero)
import Numeric.Data.Positive (Positive, unsafePositive)
import Test.TestBounds (TestBounds (maxVal, minVal))

integer :: Gen Integer
integer = HG.integral $ HR.exponentialFrom minVal 0 maxVal

natural :: Gen Natural
natural = HG.integral $ HR.exponential minVal maxVal

fraction :: Gen (Fraction Integer)
fraction = unsafeFraction <$> integer <*> integerNZ

modN :: Gen (ModN 10 Natural)
modN = mkModN <$> natural

modP :: Gen (ModP 17 Natural)
modP = reallyUnsafeModP <$> natural

nonNegative :: Gen (NonNegative Natural)
nonNegative = unsafeNonNegative <$> natural

nonZero :: Gen (NonZero Integer)
nonZero = unsafeNonZero <$> integerNZ

positive :: Gen (Positive Integer)
positive = unsafePositive <$> pos
  where
    pos = HG.integral $ HR.exponential 1 maxVal

integerNZ :: Gen Integer
integerNZ = nzBounds HG.integral minVal maxVal

naturalNZ :: Gen Natural
naturalNZ = HG.integral $ HR.exponential 1 maxVal

fractionNonZero :: Gen (Fraction Integer)
fractionNonZero = unsafeFraction <$> integerNZ <*> integerNZ

modPNonZero :: Gen (ModP 17 Natural)
modPNonZero = reallyUnsafeModP <$> pos
  where
    pos = HG.filter (\x -> x `mod` 17 /= 0) $ HG.integral $ HR.exponential 1 maxVal

nonNegativeNonZero :: Gen (NonNegative Natural)
nonNegativeNonZero = unsafeNonNegative <$> naturalNZ

positiveNonZero :: Gen (Positive Natural)
positiveNonZero = unsafePositive <$> naturalNZ

nzBounds :: (Integral a) => (Range a -> Gen a) -> a -> a -> Gen a
nzBounds gen lower upper =
  HG.choice
    [ gen (HR.exponential lower -1),
      gen (HR.exponential 1 upper)
    ]
