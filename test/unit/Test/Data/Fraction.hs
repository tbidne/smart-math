module Test.Data.Fraction (props) where

import Data.Text.Display qualified as D
import Gens qualified
import Hedgehog ((===))
import Hedgehog qualified as H
import Numeric.Data.Fraction (Fraction ((:%:)), (%!))
import Numeric.Data.Fraction qualified as Frac
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.HUnit ((@=?))
import Test.Tasty.HUnit qualified as HUnit
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Numeric.Data.Fraction"
    [ eqProps,
      reduceProps,
      numProps,
      numeratorProp,
      denominatorProp,
      testUnsafe,
      testRecip,
      showSpecs,
      displaySpecs
    ]

eqProps :: TestTree
eqProps =
  T.testGroup
    "Equality"
    [ eqMult,
      eqZero
    ]

eqMult :: TestTree
eqMult =
  Utils.testPropertyCompat "n :%: d === n * k :%: d * k" "eqMult" $
    H.property $ do
      x@(n :%: d) <- H.forAll Gens.fraction
      k <- H.forAll Gens.integerNZ
      x === (n * k) %! (d * k)

eqZero :: TestTree
eqZero =
  Utils.testPropertyCompat "0 :%: d1 === 0 :%: d2" "eqZero" $
    H.property $ do
      d1 <- H.forAll Gens.integerNZ
      d2 <- H.forAll Gens.integerNZ
      0 %! d1 === 0 %! d2

reduceProps :: TestTree
reduceProps =
  T.testGroup
    "Reduction"
    [ reduceIdempotent,
      reducePosDenom,
      reduceGCD1
    ]

reduceIdempotent :: TestTree
reduceIdempotent =
  Utils.testPropertyCompat "reduce x = reduce (reduce x)" "reduceIdempotent" $
    H.property $ do
      x <- H.forAll Gens.fraction
      Frac.reduce x === Frac.reduce (Frac.reduce x)

reducePosDenom :: TestTree
reducePosDenom =
  Utils.testPropertyCompat "denom (reduce x) > 0" "reducePosDenom" $
    H.property $ do
      x <- H.forAll Gens.fraction
      H.diff x ((>) . Frac.denominator) 0

reduceGCD1 :: TestTree
reduceGCD1 =
  Utils.testPropertyCompat "gcd n d <= 1" "reduceGCD1" $
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
numAddReduces =
  Utils.testPropertyCompat "(+) is reduced" "numAddReduces" $
    H.property $ do
      x <- H.forAll Gens.fraction
      y <- H.forAll Gens.fraction
      let s = x + y
      H.annotateShow s
      H.assert $ isReduced s

numSubReduces :: TestTree
numSubReduces =
  Utils.testPropertyCompat "(-) is reduced" "numSubReduces" $
    H.property $ do
      x <- H.forAll Gens.fraction
      y <- H.forAll Gens.fraction
      let s = x - y
      H.annotateShow s
      H.assert $ isReduced s

numMultReduces :: TestTree
numMultReduces =
  Utils.testPropertyCompat "(*) is reduced" "numMultReduces" $
    H.property $ do
      x <- H.forAll Gens.fraction
      y <- H.forAll Gens.fraction
      let s = x * y
      H.annotateShow s
      H.assert $ isReduced s

absGtZero :: TestTree
absGtZero =
  Utils.testPropertyCompat "negate . negate === id" "absGtZero" $
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
signumProp =
  Utils.testPropertyCompat "negate . negate === id" "signumProp" $
    H.property $ do
      x <- H.forAll Gens.fraction
      if
        | x > 0 -> 1 === signum x
        | x == 0 -> 0 === signum x
        | otherwise -> -1 === signum x

numeratorProp :: TestTree
numeratorProp =
  Utils.testPropertyCompat "numerator x@(n :%: _) === n" "numeratorProp" $
    H.property $ do
      x@(n :%: _) <- H.forAll Gens.fraction
      n === Frac.numerator x

denominatorProp :: TestTree
denominatorProp =
  Utils.testPropertyCompat "denominator x@(_ :%: d) === d" "denominatorProp" $
    H.property $ do
      x@(_ :%: d) <- H.forAll Gens.fraction
      d === Frac.denominator x

isReduced :: (Integral a) => Fraction a -> Bool
isReduced (0 :%: d) = d == 1
isReduced x@(_ :%: d)
  | d < 0 = False
  | otherwise = fracGcd x == 1

fracGcd :: (Integral a) => Fraction a -> a
fracGcd (n :%: d) = gcd n d

testUnsafe :: TestTree
testUnsafe = HUnit.testCase "Test unsafeFraction" $ do
  1 %! 2 @=? Frac.unsafeFraction @Integer 5 10

  Utils.assertPureErrorCall expectedEx (Frac.unsafeFraction @Integer 5 0)
  where
    expectedEx = "Numeric.Data.Fraction.unsafeFraction: Fraction has zero denominator"

testRecip :: TestTree
testRecip = HUnit.testCase "Test recip" $ do
  1 %! 2 @=? recip (2 %! (1 :: Integer))

  Utils.assertPureErrorCall expectedEx (recip $ 0 %! (2 :: Integer))
  where
    expectedEx = "Numeric.Data.Fraction.recip: Fraction has zero numerator"

showSpecs :: TestTree
showSpecs = HUnit.testCase "Shows fractions" $ do
  "UnsafeFraction 1 5" @=? show (Frac.unsafeFraction @Integer 2 10)
  "UnsafeFraction (-1) 1" @=? show (Frac.unsafeFraction @Integer 1 (-1))
  "Just (UnsafeFraction 5 7)" @=? show (Just $ Frac.unsafeFraction @Integer 5 7)

displaySpecs :: TestTree
displaySpecs = HUnit.testCase "Displays fractions" $ do
  "1 / 5" @=? D.display (Frac.unsafeFraction @Integer 2 10)
  "-1 / 1" @=? D.display (Frac.unsafeFraction @Integer 1 (-1))
  "-2340923 / 2095420" @=? D.display (Frac.unsafeFraction @Integer (-2340923) 2095420)
