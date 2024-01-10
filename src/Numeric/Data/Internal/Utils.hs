module Numeric.Data.Internal.Utils
  ( -- * Safe modular arithmetic
    checkModBound,
    modSafeAdd,
    modSafeMult,
    modSafeSub,
  )
where

import Data.Bounds
  ( AnyLowerBounded (someLowerBound),
    AnyUpperBounded (someUpperBound),
  )
import Data.Typeable (Typeable)
import Data.Typeable qualified as Typeable

-- | Verifies that the type A is large enough to fit the modulus.
-- Returns 'Nothing' if the check succeeds or a String error message if
-- the check fails.
checkModBound ::
  forall a.
  ( AnyUpperBounded a,
    Integral a,
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
  someUpperBound @a >>= \maxA ->
    let maxAInt = toInteger maxA
     in if maxS <= maxAInt
          then Nothing
          else
            Just $
              mconcat
                [ "Type '",
                  show typeA,
                  "' has a maximum size of ",
                  show maxAInt,
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
  ( AnyUpperBounded a,
    Integral a
  ) =>
  -- | x
  a ->
  -- | y
  a ->
  -- | n (modulus)
  a ->
  a
modSafeAdd = modSafeInc (+)

-- | Performs modular multiplication, accounting for rounding in the type
-- itself.
modSafeMult ::
  forall a.
  ( AnyUpperBounded a,
    Integral a
  ) =>
  -- | x
  a ->
  -- | y
  a ->
  -- | n (modulus)
  a ->
  a
modSafeMult = modSafeInc (*)

modSafeInc ::
  forall a.
  ( AnyUpperBounded a,
    Integral a
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
modSafeInc op x y modulus = case someUpperBound @a of
  -- 1. A is unbounded: Easy
  Nothing -> (x `op` y) `mod` modulus
  Just maxA ->
    let maxAInt = aToInteger maxA
        resultInt = aToInteger x `op` aToInteger y
     in if resultInt <= maxAInt
          then -- 2. A is bounded but the result fits within the bound:
          -- No problem, just convert and reduce.
            integerToA resultInt `mod` modulus
          else -- 3. Result does not fit within A. Do the modular arithmetic
          -- in Integer instead, converting the result. Note that this assumes
          -- that the final result fits within A.

            let modulusInt = aToInteger modulus
             in integerToA (resultInt `mod` modulusInt)
  where
    aToInteger :: a -> Integer
    aToInteger = toInteger

    integerToA :: Integer -> a
    integerToA = fromInteger

modSafeSub ::
  forall a.
  ( AnyLowerBounded a,
    Integral a
  ) =>
  -- | x
  a ->
  -- | y
  a ->
  -- | n (modulus)
  a ->
  a
modSafeSub x y modulus = case someLowerBound @a of
  -- 1. A is unbounded: Easy
  Nothing -> (x - y) `mod` modulus
  Just minA ->
    let minAInt = aToInteger minA
        diffInt = aToInteger x - aToInteger y
     in if diffInt >= minAInt
          then -- 2. A is bounded but the result fits within the bound:
          -- No problem, just convert and reduce.
            integerToA diffInt `mod` modulus
          else -- 3. Result does not fit within A. Do the modular arithmetic
          -- in Integer instead, converting the result. Note that this assumes
          -- that the final result fits within A. else modulus - y + x

            let modulusInt = aToInteger modulus
             in integerToA (diffInt `mod` modulusInt)
  where
    aToInteger :: a -> Integer
    aToInteger = toInteger

    integerToA :: Integer -> a
    integerToA = fromInteger
