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
    anonNegative,
    bnonNegative,
    anonZero,
    bnonZero,
    apositive,
    bpositive,

    -- * NonZero

    -- ** Specializations
    integerNZ,
    fractionNonZero,
    modPNonZero,
    anonNegativeNonZero,
    bnonNegativeNonZero,
    apositiveNonZero,
    bpositiveNonZero,
  )
where

import GHC.Natural (Natural)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as HG
import Hedgehog.Range (Range)
import Hedgehog.Range qualified as HR
import Numeric.Data.Fraction (Fraction, unsafeFraction)
import Numeric.Data.ModN (ModN, unsafeModN)
import Numeric.Data.ModP (ModP, reallyUnsafeModP)
import Numeric.Data.NonNegative.Algebra qualified as ANonNeg
import Numeric.Data.NonNegative.Base qualified as BNonNeg
import Numeric.Data.NonZero.Algebra qualified as ANonZero
import Numeric.Data.NonZero.Base qualified as BNonZero
import Numeric.Data.Positive.Algebra qualified as APos
import Numeric.Data.Positive.Base qualified as BPos
import Test.TestBounds (TestBounds (maxVal, minVal))

integer :: Gen Integer
integer = HG.integral $ HR.exponentialFrom minVal 0 maxVal

natural :: Gen Natural
natural = HG.integral $ HR.exponential minVal maxVal

fraction :: Gen (Fraction Integer)
fraction = unsafeFraction <$> integer <*> integerNZ

modN :: Gen (ModN 10 Natural)
modN = unsafeModN <$> natural

modP :: Gen (ModP 17 Natural)
modP = reallyUnsafeModP <$> natural

anonNegative :: Gen (ANonNeg.NonNegative Natural)
anonNegative = ANonNeg.unsafeNonNegative <$> natural

bnonNegative :: Gen (BNonNeg.NonNegative Natural)
bnonNegative = BNonNeg.unsafeNonNegative <$> natural

anonZero :: Gen (ANonZero.NonZero Integer)
anonZero = ANonZero.unsafeNonZero <$> integerNZ

bnonZero :: Gen (BNonZero.NonZero Integer)
bnonZero = BNonZero.unsafeNonZero <$> integerNZ

apositive :: Gen (APos.Positive Integer)
apositive = APos.unsafePositive <$> pos
  where
    pos = HG.integral $ HR.exponential 1 maxVal

bpositive :: Gen (BPos.Positive Integer)
bpositive = BPos.unsafePositive <$> pos
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

anonNegativeNonZero :: Gen (ANonNeg.NonNegative Natural)
anonNegativeNonZero = ANonNeg.unsafeNonNegative <$> naturalNZ

bnonNegativeNonZero :: Gen (BNonNeg.NonNegative Natural)
bnonNegativeNonZero = BNonNeg.unsafeNonNegative <$> naturalNZ

apositiveNonZero :: Gen (BPos.Positive Natural)
apositiveNonZero = BPos.unsafePositive <$> naturalNZ

bpositiveNonZero :: Gen (BPos.Positive Natural)
bpositiveNonZero = BPos.unsafePositive <$> naturalNZ

nzBounds :: (Integral a) => (Range a -> Gen a) -> a -> a -> Gen a
nzBounds gen lower upper =
  HG.choice
    [ gen (HR.exponential lower -1),
      gen (HR.exponential 1 upper)
    ]
