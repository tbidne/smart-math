module Test.Data.Fraction (props) where

import Data.Text.Display qualified as D
import Gens qualified
import Numeric.Data.Fraction (Fraction ((:%:)), (%!), _MkFraction)
import Numeric.Data.Fraction qualified as Frac
import Test.Prelude
import Utils qualified

props :: TestTree
props =
  testGroup
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
  testGroup
    "Equality"
    [ eqMult,
      eqZero
    ]

eqMult :: TestTree
eqMult =
  testPropertyCompat "n :%: d === n * k :%: d * k" "eqMult" $
    property $ do
      x@(n :%: d) <- forAll Gens.fraction
      k <- forAll Gens.integerNZ
      x === (n * k) %! (d * k)

eqZero :: TestTree
eqZero =
  testPropertyCompat "0 :%: d1 === 0 :%: d2" "eqZero" $
    property $ do
      d1 <- forAll Gens.integerNZ
      d2 <- forAll Gens.integerNZ
      0 %! d1 === 0 %! d2

reduceProps :: TestTree
reduceProps =
  testGroup
    "Reduction"
    [ reduceIdempotent,
      reducePosDenom,
      reduceGCD1
    ]

reduceIdempotent :: TestTree
reduceIdempotent =
  testPropertyCompat "reduce x = reduce (reduce x)" "reduceIdempotent" $
    property $ do
      x <- forAll Gens.fraction
      Frac.reduce x === Frac.reduce (Frac.reduce x)

reducePosDenom :: TestTree
reducePosDenom =
  testPropertyCompat "denom (reduce x) > 0" "reducePosDenom" $
    property $ do
      x <- forAll Gens.fraction
      diff x ((>) . Frac.denominator) 0

reduceGCD1 :: TestTree
reduceGCD1 =
  testPropertyCompat "gcd n d <= 1" "reduceGCD1" $
    property $ do
      x <- forAll Gens.fraction
      diff x ((<=) . fracGcd) 1

numProps :: TestTree
numProps =
  testGroup
    "Num"
    [ numAddReduces,
      numSubReduces,
      numMultReduces,
      absGtZero,
      signumProp
    ]

numAddReduces :: TestTree
numAddReduces =
  testPropertyCompat "(+) is reduced" "numAddReduces" $
    property $ do
      x <- forAll Gens.fraction
      y <- forAll Gens.fraction
      let s = x + y
      annotateShow s
      assert $ isReduced s

numSubReduces :: TestTree
numSubReduces =
  testPropertyCompat "(-) is reduced" "numSubReduces" $
    property $ do
      x <- forAll Gens.fraction
      y <- forAll Gens.fraction
      let s = x - y
      annotateShow s
      assert $ isReduced s

numMultReduces :: TestTree
numMultReduces =
  testPropertyCompat "(*) is reduced" "numMultReduces" $
    property $ do
      x <- forAll Gens.fraction
      y <- forAll Gens.fraction
      let s = x * y
      annotateShow s
      assert $ isReduced s

absGtZero :: TestTree
absGtZero =
  testPropertyCompat "negate . negate === id" "absGtZero" $
    property $ do
      x <- forAll Gens.fraction
      y <- forAll Gens.fraction

      -- idempotence: |x| = ||x||
      abs x === abs (abs x)

      -- non-negative: |x| >= 0
      diff (abs x) (>=) 0

      -- triangle equality: |x + y| <= |x| + |y|
      diff (abs (x + y)) (<=) (abs x + abs y)

signumProp :: TestTree
signumProp =
  testPropertyCompat "negate . negate === id" "signumProp" $
    property $ do
      x <- forAll Gens.fraction
      if
        | x > 0 -> 1 === signum x
        | x == 0 -> 0 === signum x
        | otherwise -> -1 === signum x

numeratorProp :: TestTree
numeratorProp =
  testPropertyCompat "numerator x@(n :%: _) === n" "numeratorProp" $
    property $ do
      x@(n :%: _) <- forAll Gens.fraction
      n === Frac.numerator x
      n === x.numerator
      n === view #numerator x
      n === view (_MkFraction % _1) x

denominatorProp :: TestTree
denominatorProp =
  testPropertyCompat "denominator x@(_ :%: d) === d" "denominatorProp" $
    property $ do
      x@(_ :%: d) <- forAll Gens.fraction
      d === Frac.denominator x
      d === x.denominator
      d === view #denominator x
      d === view (_MkFraction % _2) x

isReduced :: (Integral a) => Fraction a -> Bool
isReduced (0 :%: d) = d == 1
isReduced x@(_ :%: d)
  | d < 0 = False
  | otherwise = fracGcd x == 1

fracGcd :: (Integral a) => Fraction a -> a
fracGcd (n :%: d) = gcd n d

testUnsafe :: TestTree
testUnsafe = testCase "Test unsafeFraction" $ do
  1 %! 2 @=? Frac.unsafeFraction @Integer 5 10

  Utils.assertPureErrorCall expectedEx (Frac.unsafeFraction @Integer 5 0)
  where
    expectedEx = "Numeric.Data.Fraction.unsafeFraction: Fraction has zero denominator"

testRecip :: TestTree
testRecip = testCase "Test recip" $ do
  1 %! 2 @=? recip (2 %! (1 :: Integer))

  Utils.assertPureErrorCall expectedEx (recip $ 0 %! (2 :: Integer))
  where
    expectedEx = "Numeric.Data.Fraction.recip: Fraction has zero numerator"

showSpecs :: TestTree
showSpecs = testCase "Shows fractions" $ do
  "UnsafeFraction 1 5" @=? show (Frac.unsafeFraction @Integer 2 10)
  "UnsafeFraction (-1) 1" @=? show (Frac.unsafeFraction @Integer 1 (-1))
  "Just (UnsafeFraction 5 7)" @=? show (Just $ Frac.unsafeFraction @Integer 5 7)

displaySpecs :: TestTree
displaySpecs = testCase "Displays fractions" $ do
  "1 / 5" @=? D.display (Frac.unsafeFraction @Integer 2 10)
  "-1 / 1" @=? D.display (Frac.unsafeFraction @Integer 1 (-1))
  "-2340923 / 2095420" @=? D.display (Frac.unsafeFraction @Integer (-2340923) 2095420)
