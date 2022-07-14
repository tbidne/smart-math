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

import Data.Functor.Identity (Identity)
import GHC.Natural (Natural)
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as HG
import Hedgehog.Range (Range)
import Hedgehog.Range qualified as HR
import Numeric.Algebra.Multiplicative.MGroup qualified as MGroup
import Numeric.Data.Fraction (Fraction (..), unsafeFraction)
import Numeric.Data.ModN (ModN (..), mkModN)
import Numeric.Data.ModP (ModP (..), reallyUnsafeModP)
import Numeric.Data.NonNegative (NonNegative (..), unsafeNonNegative)
import Numeric.Data.NonZero (NonZero (..), unsafeNonZero)
import Numeric.Data.Positive (Positive (..), unsafePositive)
import Numeric.Data.Positive qualified as Pos
import Test.TestBounds (TestBounds (..))

integer :: MonadGen m => m Integer
integer = HG.integral $ HR.exponentialFrom minVal 0 maxVal

natural :: MonadGen m => m Natural
natural = HG.integral $ HR.exponential minVal maxVal

fraction :: MonadGen m => m (Fraction Integer)
fraction = unsafeFraction <$> integer <*> integerNZ

modN :: MonadGen m => m (ModN 10 Natural)
modN = mkModN <$> natural

modP :: MonadGen m => m (ModP 17 Natural)
modP = reallyUnsafeModP <$> natural

nonNegative :: MonadGen m => m (NonNegative Natural)
nonNegative = unsafeNonNegative <$> natural

nonZero :: MonadGen m => m (NonZero Integer)
nonZero = unsafeNonZero <$> integerNZ

positive :: MonadGen m => m (Positive Integer)
positive = unsafePositive <$> pos
  where
    pos = HG.integral $ HR.exponential 1 maxVal

integerNZ :: MonadGen m => m Integer
integerNZ = nzBounds HG.integral minVal maxVal

naturalNZ :: MonadGen m => m Natural
naturalNZ = HG.integral $ HR.exponential 1 maxVal

fractionNonZero :: MonadGen m => m (NonZero (Fraction Integer))
fractionNonZero = fmap MGroup.unsafeAMonoidNonZero $ unsafeFraction <$> integerNZ <*> integerNZ

modPNonZero :: (GenBase m ~ Identity, MonadGen m) => m (NonZero (ModP 17 Natural))
modPNonZero = MGroup.unsafeAMonoidNonZero . reallyUnsafeModP <$> pos
  where
    pos = HG.filter (\x -> x `mod` 17 /= 0) $ HG.integral $ HR.exponential 1 maxVal

nonNegativeNonZero :: MonadGen m => m (NonZero (NonNegative Natural))
nonNegativeNonZero = MGroup.unsafeAMonoidNonZero . unsafeNonNegative <$> naturalNZ

positiveNonZero :: MonadGen m => m (NonZero (Positive Natural))
positiveNonZero = Pos.positiveToNonZero . unsafePositive <$> naturalNZ

nzBounds :: (Integral a, MonadGen m) => (Range a -> m a) -> a -> a -> m a
nzBounds gen lower upper =
  HG.choice
    [ gen (HR.exponential lower -1),
      gen (HR.exponential 1 upper)
    ]
