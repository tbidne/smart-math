-- | This module reexports smart-constructors for enforcing mathematical
-- invariants.
--
-- @since 0.1.0.0
module Numeric.Data
  ( module Numeric.Data.Fraction,
    module Numeric.Data.ModN,
    module Numeric.Data.ModP,
    module Numeric.Data.NonNegative,
    module Numeric.Data.NonZero,
    module Numeric.Data.Positive,
  )
where

import Numeric.Data.Fraction
import Numeric.Data.ModN
import Numeric.Data.ModP
import Numeric.Data.NonNegative
import Numeric.Data.NonZero
import Numeric.Data.Positive
