-- | Provides the 'Division' typeclass for abstracting over division.
--
-- @since 0.1.0.0
module Numeric.Class.Division
  ( Division (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import GHC.Real (Ratio)

-- | Abstracts over division operators.
--
-- @since 0.1.0.0
class Division a where
  divide :: a -> a -> a

-- | @since 0.1.0.0
instance Division Float where
  divide = (/)

-- | @since 0.1.0.0
instance Division Double where
  divide = (/)

-- | @since 0.1.0.0
instance Division Int where
  divide = div

-- | @since 0.1.0.0
instance Division Int8 where
  divide = div

-- | @since 0.1.0.0
instance Division Int16 where
  divide = div

-- | @since 0.1.0.0
instance Division Int32 where
  divide = div

-- | @since 0.1.0.0
instance Division Int64 where
  divide = div

-- | @since 0.1.0.0
instance Division Integer where
  divide = div

-- | @since 0.1.0.0
instance Division Word where
  divide = div

-- | @since 0.1.0.0
instance Division Word8 where
  divide = div

-- | @since 0.1.0.0
instance Division Word16 where
  divide = div

-- | @since 0.1.0.0
instance Division Word32 where
  divide = div

-- | @since 0.1.0.0
instance Division Word64 where
  divide = div

-- | @since 0.1.0.0
instance Division Natural where
  divide = div

-- | @since 0.1.0.0
instance Division (Ratio Integer) where
  divide = (/)

-- | @since 0.1.0.0
instance Division (Ratio Natural) where
  divide = (/)
