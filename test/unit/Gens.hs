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
import Numeric.Data.Fraction qualified as AFrac
import Numeric.Data.ModN qualified as AModN
import Numeric.Data.ModP qualified as AModP
import Numeric.Data.NonNegative qualified as ANonNeg
import Numeric.Data.NonZero qualified as ANonZero
import Numeric.Data.Positive qualified as APos
import Numeric.Data.Positive qualified as BPos
import Test.TestBounds (TestBounds (maxVal, minVal))

integer :: Gen Integer
integer = HG.integral $ HR.exponentialFrom minVal 0 maxVal

natural :: Gen Natural
natural = HG.integral $ HR.exponential minVal maxVal

fraction :: Gen (AFrac.Fraction Integer)
fraction = AFrac.unsafeFraction <$> integer <*> integerNZ

modN :: Gen (AModN.ModN 10 Natural)
modN = AModN.unsafeModN <$> natural

modP :: Gen (AModP.ModP 17 Natural)
modP = AModP.reallyUnsafeModP <$> natural

nonNegative :: Gen (ANonNeg.NonNegative Natural)
nonNegative = ANonNeg.unsafeNonNegative <$> natural

nonZero :: Gen (ANonZero.NonZero Integer)
nonZero = ANonZero.unsafeNonZero <$> integerNZ

positive :: Gen (APos.Positive Integer)
positive = APos.unsafePositive <$> pos
  where
    pos = HG.integral $ HR.exponential 1 maxVal

integerNZ :: Gen Integer
integerNZ = nzBounds HG.integral minVal maxVal

naturalNZ :: Gen Natural
naturalNZ = HG.integral $ HR.exponential 1 maxVal

fractionNonZero :: Gen (AFrac.Fraction Integer)
fractionNonZero = AFrac.unsafeFraction <$> integerNZ <*> integerNZ

modPNonZero :: Gen (AModP.ModP 17 Natural)
modPNonZero = AModP.reallyUnsafeModP <$> pos
  where
    pos = HG.filter (\x -> x `mod` 17 /= 0) $ HG.integral $ HR.exponential 1 maxVal

nonNegativeNonZero :: Gen (ANonNeg.NonNegative Natural)
nonNegativeNonZero = ANonNeg.unsafeNonNegative <$> naturalNZ

positiveNonZero :: Gen (BPos.Positive Natural)
positiveNonZero = BPos.unsafePositive <$> naturalNZ

nzBounds :: (Integral a) => (Range a -> Gen a) -> a -> a -> Gen a
nzBounds gen lower upper =
  HG.choice
    [ gen (HR.exponential lower -1),
      gen (HR.exponential 1 upper)
    ]
