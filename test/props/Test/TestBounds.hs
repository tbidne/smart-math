-- | Provides the 'TestBounds' type.
--
-- @since 0.1
module Test.TestBounds
  ( TestBounds (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)

-- | Common interface for defining what bounds we should use during testing.
--
-- @since 0.1
class TestBounds a where
  minVal :: a
  maxVal :: a

-- | @since 0.1
instance TestBounds Float where
  minVal = -3.402823e+38
  maxVal = 3.402823e+38

-- | @since 0.1
instance TestBounds Double where
  minVal = -1.8e308
  maxVal = 1.8e308

-- | @since 0.1
instance TestBounds Integer where
  minVal = floor @Double -2e40
  maxVal = floor @Double 2e40

-- | @since 0.1
instance TestBounds Natural where
  minVal = 0
  maxVal = floor @Double 2e40

-- | @since 0.1
instance TestBounds Int where
  minVal = minBound
  maxVal = maxBound

-- | @since 0.1
instance TestBounds Int8 where
  minVal = minBound
  maxVal = maxBound

-- | @since 0.1
instance TestBounds Int16 where
  minVal = minBound
  maxVal = maxBound

-- | @since 0.1
instance TestBounds Int32 where
  minVal = minBound
  maxVal = maxBound

-- | @since 0.1
instance TestBounds Int64 where
  minVal = minBound
  maxVal = maxBound

-- | @since 0.1
instance TestBounds Word where
  minVal = minBound
  maxVal = maxBound

-- | @since 0.1
instance TestBounds Word8 where
  minVal = minBound
  maxVal = maxBound

-- | @since 0.1
instance TestBounds Word16 where
  minVal = minBound
  maxVal = maxBound

-- | @since 0.1
instance TestBounds Word32 where
  minVal = minBound
  maxVal = maxBound

-- | @since 0.1
instance TestBounds Word64 where
  minVal = minBound
  maxVal = maxBound
