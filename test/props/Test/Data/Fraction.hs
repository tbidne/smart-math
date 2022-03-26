module Test.Data.Fraction (props) where

import Gens qualified
import Hedgehog ((===))
import Hedgehog qualified as H
import MaxRuns (MaxRuns (..))
import Numeric.Class.Boundless (UpperBoundless)
import Numeric.Data.Fraction (Fraction (..))
import Numeric.Data.Fraction qualified as Frac
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Numeric.Data.Fraction"
    [ eqProps,
      reduceProps,
      numProps,
      showRoundTrip,
      numeratorProp,
      denominatorProp
    ]

eqProps :: TestTree
eqProps =
  T.testGroup
    "Equality"
    [ eqMult,
      eqZero
    ]

eqMult :: TestTree
eqMult = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "n :%: d === n * k :%: d * k" "eqMult" $
    H.withTests limit $
      H.property $ do
        x@(n :%: d) <- H.forAll Gens.fraction
        k <- H.forAll Gens.integerNZ
        x === (n * k :%: d * k)

eqZero :: TestTree
eqZero = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "0 :%: d1 === 0 :%: d2" "eqZero" $
    H.withTests limit $
      H.property $ do
        d1 <- H.forAll Gens.integerNZ
        d2 <- H.forAll Gens.integerNZ
        (0 :%: d1) === (0 :%: d2)

reduceProps :: TestTree
reduceProps =
  T.testGroup
    "Reduction"
    [ reduceIdempotent,
      reducePosDenom,
      reduceGCD1
    ]

reduceIdempotent :: TestTree
reduceIdempotent = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "reduce x = reduce (reduce x)" "reduceIdempotent" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.fraction
        Frac.reduce x === Frac.reduce (Frac.reduce x)

reducePosDenom :: TestTree
reducePosDenom = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "denom (reduce x) > 0" "reducePosDenom" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.fraction
        H.diff x ((>) . Frac.denominator) 0

reduceGCD1 :: TestTree
reduceGCD1 = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "gcd n d <= 1" "reduceGCD1" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.fraction
        H.diff x ((<=) . fracGcd) 1

numProps :: TestTree
numProps =
  T.testGroup
    "Num"
    [ numAddReduces,
      numSubReduces,
      numMultReduces,
      absGtZero,
      signumProp
    ]

numAddReduces :: TestTree
numAddReduces = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "(+) is reduced" "numAddReduces" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.fraction
        y <- H.forAll Gens.fraction
        let s = x + y
        H.annotateShow s
        H.assert $ isReduced s

numSubReduces :: TestTree
numSubReduces = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "(-) is reduced" "numSubReduces" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.fraction
        y <- H.forAll Gens.fraction
        let s = x - y
        H.annotateShow s
        H.assert $ isReduced s

numMultReduces :: TestTree
numMultReduces = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "(*) is reduced" "numMultReduces" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.fraction
        y <- H.forAll Gens.fraction
        let s = x * y
        H.annotateShow s
        H.assert $ isReduced s

absGtZero :: TestTree
absGtZero = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "negate . negate === id" "absGtZero" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.fraction
        y <- H.forAll Gens.fraction

        -- idempotence: |x| = ||x||
        abs x === abs (abs x)

        -- non-negative: |x| >= 0
        H.diff (abs x) (>=) 0

        -- triangle equality: |x + y| <= |x| + |y|
        H.diff (abs (x + y)) (<=) (abs x + abs y)

signumProp :: TestTree
signumProp = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "negate . negate === id" "signumProp" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.fraction
        if
            | x > 0 -> 1 === signum x
            | x == 0 -> 0 === signum x
            | otherwise -> -1 === signum x

showRoundTrip :: TestTree
showRoundTrip = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "read . show === id" "showRoundTrip" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.fraction
        x === read (show x)

numeratorProp :: TestTree
numeratorProp = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "numerator x@(n :%: _) === n" "numeratorProp" $
    H.withTests limit $
      H.property $ do
        x@(n :%: _) <- H.forAll Gens.fraction
        n === Frac.numerator x

denominatorProp :: TestTree
denominatorProp = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "denominator x@(_ :%: d) === d" "denominatorProp" $
    H.withTests limit $
      H.property $ do
        x@(_ :%: d) <- H.forAll Gens.fraction
        d === Frac.denominator x

isReduced :: UpperBoundless a => Fraction a -> Bool
isReduced (0 :%: d) = d == 1
isReduced x@(_ :%: d)
  | d < 0 = False
  | otherwise = fracGcd x == 1

fracGcd :: UpperBoundless a => Fraction a -> a
fracGcd (n :%: d) = gcd n d
