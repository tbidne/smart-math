-- | Provides typeclass for grouping types together based on their bounded
-- behavior.
--
-- @since 0.1.0.0
module Numeric.Class.Boundless
  ( UpperBoundless,
  )
where

import GHC.Natural (Natural)

-- | Types that have no upper bound. Used for when a function's correctness
-- depends on the type being unbounded.
--
-- @since 0.0.1.0
class Integral a => UpperBoundless a

-- | @since 0.0.1.0
instance UpperBoundless Integer

-- | @since 0.0.1.0
instance UpperBoundless Natural
