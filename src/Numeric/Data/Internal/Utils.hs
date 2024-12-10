-- | Internal utils.
module Numeric.Data.Internal.Utils
  ( -- * Safe modular arithmetic

    -- ** Algebra-simple
    checkModBoundAlgebra,
    modSafeAddAlgebra,
    modSafeMultAlgebra,
    modSafeSubAlgebra,

    -- ** Base
    checkModBound,
    modSafeAdd,
    modSafeMult,
    modSafeSub,

    -- * Subtraction
    safeDiff,

    -- * Optics
    rmatching,
  )
where

import Data.Bounds
  ( MaybeLowerBounded (maybeLowerBound),
    MaybeUpperBounded (maybeUpperBound),
  )
import Data.Typeable (Typeable)
import Data.Typeable qualified as Typeable
import Numeric.Algebra
  ( ASemigroup ((.+.)),
    MEuclidean,
    MSemigroup ((.*.)),
    mmod,
  )
import Numeric.Convert.Integer (FromInteger (fromZ), ToInteger (toZ))
import Optics.Core
  ( An_AffineTraversal,
    Is,
    NoIx,
    Optic,
    ReversibleOptic (ReversedOptic),
    matching,
    re,
  )

-- | Verifies that the type A is large enough to fit the modulus.
-- Returns 'Nothing' if the check succeeds or a String error message if
-- the check fails.
checkModBound ::
  forall a.
  ( Integral a,
    MaybeUpperBounded a,
    Typeable a
  ) =>
  -- | The type /a/ whose upper bound must be large enough to accommodate
  -- modular arithmetic within modulus /n/. This would be a Proxy except we get
  -- a better error message for the value itself.
  a ->
  -- | The modulus n that should satisfy @n <= max(a)@.
  Integer ->
  Maybe String
checkModBound aTerm modulus =
  maybeUpperBound @a >>= \maxA ->
    let maxAℤ = toInteger maxA
     in if maxS <= maxAℤ
          then Nothing
          else
            Just $
              mconcat
                [ "Type '",
                  show typeA,
                  "' has a maximum size of ",
                  show maxAℤ,
                  ". This is not large enough to safely implement mod ",
                  show modulus,
                  "."
                ]
  where
    -- This should ostensibly be modulus - 1 since the highest value in
    -- Z/nZ is (n - 1). But we need to actually perform mod n, hence,
    -- type A must be >= modulus itself.
    maxS = modulus
    typeA = Typeable.typeOf aTerm

-- | Verifies that the type A is large enough to fit the modulus.
-- Returns 'Nothing' if the check succeeds or a String error message if
-- the check fails.
checkModBoundAlgebra ::
  forall a.
  ( ToInteger a,
    MaybeUpperBounded a,
    Typeable a
  ) =>
  -- | The type /a/ whose upper bound must be large enough to accommodate
  -- modular arithmetic within modulus /n/. This would be a Proxy except we get
  -- a better error message for the value itself.
  a ->
  -- | The modulus n that should satisfy @n <= max(a)@.
  Integer ->
  Maybe String
checkModBoundAlgebra aTerm modulus =
  maybeUpperBound @a >>= \maxA ->
    let maxAℤ = toZ maxA
     in if maxS <= maxAℤ
          then Nothing
          else
            Just $
              mconcat
                [ "Type '",
                  show typeA,
                  "' has a maximum size of ",
                  show maxAℤ,
                  ". This is not large enough to safely implement mod ",
                  show modulus,
                  "."
                ]
  where
    -- This should ostensibly be modulus - 1 since the highest value in
    -- Z/nZ is (n - 1). But we need to actually perform mod n, hence,
    -- type A must be >= modulus itself.
    maxS = modulus
    typeA = Typeable.typeOf aTerm

-- | Performs modular addition, accounting for rounding in the type itself.
modSafeAdd ::
  forall a.
  ( Integral a,
    MaybeUpperBounded a
  ) =>
  -- | x
  a ->
  -- | y
  a ->
  -- | n (modulus)
  a ->
  a
modSafeAdd = modSafeInc (+)

-- | Performs modular addition, accounting for rounding in the type
-- itself.
modSafeAddAlgebra ::
  forall a.
  ( ASemigroup a,
    FromInteger a,
    MEuclidean a,
    MaybeUpperBounded a,
    ToInteger a
  ) =>
  -- | x
  a ->
  -- | y
  a ->
  -- | n (modulus)
  a ->
  a
modSafeAddAlgebra x y modulus = case maybeUpperBound @a of
  -- 1. A is unbounded: Easy
  Nothing -> (x .+. y) `mmod` modulus
  Just maxA ->
    let maxAℤ = aToℤ maxA
        resultℤ = aToℤ x .+. aToℤ y
     in if resultℤ <= maxAℤ
          then -- 2. A is bounded but the result fits within the bound:
          -- No problem, just convert and reduce.
            aFromℤ resultℤ `mmod` modulus
          else -- 3. Result does not fit within A. Do the modular arithmetic
          -- in Integer instead, converting the result. Note that this assumes
          -- that the final result fits within A.
            let modulusℤ = aToℤ modulus
             in aFromℤ (resultℤ `mmod` modulusℤ)
  where
    aToℤ :: a -> Integer
    aToℤ = toZ

    aFromℤ :: Integer -> a
    aFromℤ = fromZ

-- | Performs modular multiplication, accounting for rounding in the type
-- itself.
modSafeMult ::
  forall a.
  ( Integral a,
    MaybeUpperBounded a
  ) =>
  -- | x
  a ->
  -- | y
  a ->
  -- | n (modulus)
  a ->
  a
modSafeMult = modSafeInc (*)

-- | Performs modular multiplication, accounting for rounding in the type
-- itself.
modSafeMultAlgebra ::
  forall a.
  ( FromInteger a,
    MEuclidean a,
    MaybeUpperBounded a,
    ToInteger a
  ) =>
  -- | x
  a ->
  -- | y
  a ->
  -- | n (modulus)
  a ->
  a
modSafeMultAlgebra x y modulus = case maybeUpperBound @a of
  -- 1. A is unbounded: Easy
  Nothing -> (x .*. y) `mmod` modulus
  Just maxA ->
    let maxAℤ = aToℤ maxA
        resultℤ = aToℤ x .*. aToℤ y
     in if resultℤ <= maxAℤ
          then -- 2. A is bounded but the result fits within the bound:
          -- No problem, just convert and reduce.
            aFromℤ resultℤ `mmod` modulus
          else -- 3. Result does not fit within A. Do the modular arithmetic
          -- in Integer instead, converting the result. Note that this assumes
          -- that the final result fits within A.
            let modulusℤ = aToℤ modulus
             in aFromℤ (resultℤ `mmod` modulusℤ)
  where
    aToℤ :: a -> Integer
    aToℤ = toZ

    aFromℤ :: Integer -> a
    aFromℤ = fromZ

modSafeInc ::
  forall a.
  ( Integral a,
    MaybeUpperBounded a
  ) =>
  -- | Operations (addition or multiplication)
  (forall x. (Integral x) => x -> x -> x) ->
  -- | x
  a ->
  -- | y
  a ->
  -- | n (modulus)
  a ->
  a
modSafeInc op x y modulus = case maybeUpperBound @a of
  -- 1. A is unbounded: Easy
  Nothing -> (x `op` y) `mod` modulus
  Just maxA ->
    let maxAℤ = aToℤ maxA
        resultℤ = aToℤ x `op` aToℤ y
     in if resultℤ <= maxAℤ
          then -- 2. A is bounded but the result fits within the bound:
          -- No problem, just convert and reduce.
            aFromℤ resultℤ `mod` modulus
          else -- 3. Result does not fit within A. Do the modular arithmetic
          -- in Integer instead, converting the result. Note that this assumes
          -- that the final result fits within A.
            let modulusℤ = aToℤ modulus
             in aFromℤ (resultℤ `mod` modulusℤ)
  where
    aToℤ :: a -> Integer
    aToℤ = toInteger

    aFromℤ :: Integer -> a
    aFromℤ = fromInteger

-- | Performs modular subtraction, accounting for rounding in the type
-- itself.
modSafeSub ::
  forall a.
  ( Integral a,
    MaybeLowerBounded a
  ) =>
  -- | x
  a ->
  -- | y
  a ->
  -- | n (modulus)
  a ->
  a
modSafeSub x y modulus = case maybeLowerBound @a of
  -- 1. A is unbounded: Easy
  Nothing -> (x - y) `mod` modulus
  Just minA ->
    let minAℤ = aToℤ minA
        diffℤ = aToℤ x - aToℤ y
     in if diffℤ >= minAℤ
          then -- 2. A is bounded but the result fits within the bound:
          -- No problem, just convert and reduce.
            aFromℤ diffℤ `mod` modulus
          else -- 3. Result does not fit within A. Do the modular arithmetic
          -- in Integer instead, converting the result. Note that this assumes
          -- that the final result fits within A. else modulus - y + x
            let modulusℤ = aToℤ modulus
             in aFromℤ (diffℤ `mod` modulusℤ)
  where
    aToℤ :: a -> Integer
    aToℤ = toInteger

    aFromℤ :: Integer -> a
    aFromℤ = fromInteger

-- | Performs modular subtraction, accounting for rounding in the type
-- itself.
modSafeSubAlgebra ::
  forall a.
  ( ASemigroup a,
    FromInteger a,
    MEuclidean a,
    ToInteger a,
    MaybeUpperBounded a
  ) =>
  -- | x
  a ->
  -- | y
  a ->
  -- | n (modulus)
  a ->
  a
modSafeSubAlgebra x y modulus = case maybeUpperBound @a of
  -- 1. A is unbounded: Easy
  Nothing -> (x .+. fromZ negYℤ) `mmod` modulus
  Just maxA ->
    let maxAℤ = aToℤ maxA
        resultℤ = xℤ .+. negYℤ
     in if resultℤ <= maxAℤ
          then -- 2. A is bounded but the result fits within the bound:
          -- No problem, just convert and reduce.
            aFromℤ resultℤ `mmod` modulus
          else -- 3. Result does not fit within A. Do the modular arithmetic
          -- in Integer instead, converting the result. Note that this assumes
          -- that the final result fits within A.
            aFromℤ (resultℤ `mmod` modulusℤ)
  where
    xℤ = aToℤ x
    yℤ = aToℤ y
    modulusℤ = aToℤ modulus

    negYℤ = modulusℤ - yℤ

    aToℤ :: a -> Integer
    aToℤ = toZ

    aFromℤ :: Integer -> a
    aFromℤ = fromZ

-- | Reversed 'matching'. Useful with smart-constructor optics.
--
-- @since 0.1
rmatching ::
  (Is (ReversedOptic k) An_AffineTraversal, ReversibleOptic k) =>
  Optic k NoIx b a t s ->
  s ->
  Either t a
rmatching = matching . re
{-# INLINEABLE rmatching #-}

safeDiff :: (Real a) => a -> a -> Double
safeDiff x y
  | x <= y = realToFrac $ y - x
  | otherwise = realToFrac $ x - y
