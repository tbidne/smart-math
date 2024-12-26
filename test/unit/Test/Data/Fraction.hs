module Test.Data.Fraction (props) where

import Data.Text.Display qualified as D
import Gens qualified
import Numeric.Data.Fraction.Algebra qualified as AFrac
import Numeric.Data.Fraction.Base qualified as BFrac
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
      k <- forAll Gens.integerNZ

      ax@(an AFrac.:%: ad) <- forAll Gens.afraction
      ax === (an * k) AFrac.%! (ad * k)

      bx@(bn BFrac.:%: bd) <- forAll Gens.bfraction
      bx === (bn * k) BFrac.%! (bd * k)

eqZero :: TestTree
eqZero =
  testPropertyCompat "0 :%: d1 === 0 :%: d2" "eqZero" $
    property $ do
      d1 <- forAll Gens.integerNZ
      d2 <- forAll Gens.integerNZ
      0 AFrac.%! d1 === 0 AFrac.%! d2
      0 BFrac.%! d1 === 0 BFrac.%! d2

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
      ax <- forAll Gens.afraction
      AFrac.reduce ax === AFrac.reduce (AFrac.reduce ax)

      bx <- forAll Gens.bfraction
      BFrac.reduce bx === BFrac.reduce (BFrac.reduce bx)

reducePosDenom :: TestTree
reducePosDenom =
  testPropertyCompat "denom (reduce x) > 0" "reducePosDenom" $
    property $ do
      ax <- forAll Gens.afraction
      diff ax ((>) . AFrac.denominator) 0

      bx <- forAll Gens.bfraction
      diff bx ((>) . BFrac.denominator) 0

reduceGCD1 :: TestTree
reduceGCD1 =
  testPropertyCompat "gcd n d <= 1" "reduceGCD1" $
    property $ do
      ax <- forAll Gens.afraction
      diff ax ((<=) . afracGcd) 1

      bx <- forAll Gens.bfraction
      diff bx ((<=) . bfracGcd) 1

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
      ax <- forAll Gens.afraction
      ay <- forAll Gens.afraction
      let as = ax + ay
      annotateShow as
      assert $ aisReduced as

      bx <- forAll Gens.bfraction
      by <- forAll Gens.bfraction
      let bs = bx + by
      annotateShow bs
      assert $ bisReduced bs

numSubReduces :: TestTree
numSubReduces =
  testPropertyCompat "(-) is reduced" "numSubReduces" $
    property $ do
      ax <- forAll Gens.afraction
      ay <- forAll Gens.afraction
      let as = ax - ay
      annotateShow as
      assert $ aisReduced as

      bx <- forAll Gens.bfraction
      by <- forAll Gens.bfraction
      let bs = bx - by
      annotateShow bs
      assert $ bisReduced bs

numMultReduces :: TestTree
numMultReduces =
  testPropertyCompat "(*) is reduced" "numMultReduces" $
    property $ do
      ax <- forAll Gens.afraction
      ay <- forAll Gens.afraction
      let as = ax * ay
      annotateShow as
      assert $ aisReduced as

      bx <- forAll Gens.bfraction
      by <- forAll Gens.bfraction
      let bs = bx * by
      annotateShow bs
      assert $ bisReduced bs

absGtZero :: TestTree
absGtZero =
  testPropertyCompat "negate . negate === id" "absGtZero" $
    property $ do
      ax <- forAll Gens.afraction
      ay <- forAll Gens.afraction

      -- idempotence: |x| = ||x||
      abs ax === abs (abs ax)

      -- non-negative: |x| >= 0
      diff (abs ax) (>=) 0

      -- triangle equality: |x + y| <= |x| + |y|
      diff (abs (ax + ay)) (<=) (abs ax + abs ay)

      bx <- forAll Gens.bfraction
      by <- forAll Gens.bfraction

      -- idempotence: |x| = ||x||
      abs bx === abs (abs bx)

      -- non-negative: |x| >= 0
      diff (abs bx) (>=) 0

      -- triangle equality: |x + y| <= |x| + |y|
      diff (abs (bx + by)) (<=) (abs bx + abs by)

signumProp :: TestTree
signumProp =
  testPropertyCompat "negate . negate === id" "signumProp" $
    property $ do
      ax <- forAll Gens.afraction
      if
        | ax > 0 -> 1 === signum ax
        | ax == 0 -> 0 === signum ax
        | otherwise -> -1 === signum ax

      bx <- forAll Gens.bfraction
      if
        | bx > 0 -> 1 === signum bx
        | bx == 0 -> 0 === signum bx
        | otherwise -> -1 === signum bx

numeratorProp :: TestTree
numeratorProp =
  testPropertyCompat "numerator x@(n :%: _) === n" "numeratorProp" $
    property $ do
      ax@(an AFrac.:%: _) <- forAll Gens.afraction
      an === AFrac.numerator ax
      an === ax.numerator
      an === view #numerator ax
      an === view (AFrac._MkFraction % _1) ax

      bx@(bn BFrac.:%: _) <- forAll Gens.bfraction
      bn === BFrac.numerator bx
      bn === bx.numerator
      bn === view #numerator bx
      bn === view (BFrac._MkFraction % _1) bx

denominatorProp :: TestTree
denominatorProp =
  testPropertyCompat "denominator x@(_ :%: d) === d" "denominatorProp" $
    property $ do
      ax@(_ AFrac.:%: ad) <- forAll Gens.afraction
      ad === AFrac.denominator ax
      ad === ax.denominator
      ad === view #denominator ax
      ad === view (AFrac._MkFraction % _2) ax

      bx@(_ BFrac.:%: bd) <- forAll Gens.bfraction
      bd === BFrac.denominator bx
      bd === bx.denominator
      bd === view #denominator bx
      bd === view (BFrac._MkFraction % _2) bx

aisReduced :: (Integral a) => AFrac.Fraction a -> Bool
aisReduced (0 AFrac.:%: d) = d == 1
aisReduced x@(_ AFrac.:%: d)
  | d < 0 = False
  | otherwise = afracGcd x == 1

bisReduced :: (Integral a) => BFrac.Fraction a -> Bool
bisReduced (0 BFrac.:%: d) = d == 1
bisReduced x@(_ BFrac.:%: d)
  | d < 0 = False
  | otherwise = bfracGcd x == 1

afracGcd :: (Integral a) => AFrac.Fraction a -> a
afracGcd (n AFrac.:%: d) = gcd n d

bfracGcd :: (Integral a) => BFrac.Fraction a -> a
bfracGcd (n BFrac.:%: d) = gcd n d

testUnsafe :: TestTree
testUnsafe = testCase "Test unsafeFraction" $ do
  1 AFrac.%! 2 @=? AFrac.unsafeFraction @Integer 5 10
  Utils.assertPureErrorCall aexpectedEx (AFrac.unsafeFraction @Integer 5 0)

  1 BFrac.%! 2 @=? BFrac.unsafeFraction @Integer 5 10
  Utils.assertPureErrorCall bexpectedEx (BFrac.unsafeFraction @Integer 5 0)
  where
    aexpectedEx = "Numeric.Data.Fraction.Algebra.unsafeFraction: Fraction has zero denominator"

    bexpectedEx = "Numeric.Data.Fraction.Base.unsafeFraction: Fraction has zero denominator"

testRecip :: TestTree
testRecip = testCase "Test recip" $ do
  1 AFrac.%! 2 @=? recip (2 AFrac.%! (1 :: Integer))
  Utils.assertPureErrorCall aexpectedEx (recip $ 0 AFrac.%! (2 :: Integer))

  1 BFrac.%! 2 @=? recip (2 BFrac.%! (1 :: Integer))
  Utils.assertPureErrorCall bexpectedEx (recip $ 0 BFrac.%! (2 :: Integer))
  where
    aexpectedEx = "Numeric.Data.Fraction.Algebra.reciprocal: Fraction has zero numerator"
    bexpectedEx = "Numeric.Data.Fraction.Base.recip: Fraction has zero numerator"

showSpecs :: TestTree
showSpecs = testCase "Shows fractions" $ do
  "UnsafeFraction 1 5" @=? show (AFrac.unsafeFraction @Integer 2 10)
  "UnsafeFraction (-1) 1" @=? show (AFrac.unsafeFraction @Integer 1 (-1))
  "Just (UnsafeFraction 5 7)" @=? show (Just $ AFrac.unsafeFraction @Integer 5 7)

  "UnsafeFraction 1 5" @=? show (BFrac.unsafeFraction @Integer 2 10)
  "UnsafeFraction (-1) 1" @=? show (BFrac.unsafeFraction @Integer 1 (-1))
  "Just (UnsafeFraction 5 7)" @=? show (Just $ BFrac.unsafeFraction @Integer 5 7)

displaySpecs :: TestTree
displaySpecs = testCase "Displays fractions" $ do
  "1 / 5" @=? D.display (AFrac.unsafeFraction @Integer 2 10)
  "-1 / 1" @=? D.display (AFrac.unsafeFraction @Integer 1 (-1))
  "-2340923 / 2095420" @=? D.display (AFrac.unsafeFraction @Integer (-2340923) 2095420)

  "1 / 5" @=? D.display (BFrac.unsafeFraction @Integer 2 10)
  "-1 / 1" @=? D.display (BFrac.unsafeFraction @Integer 1 (-1))
  "-2340923 / 2095420" @=? D.display (BFrac.unsafeFraction @Integer (-2340923) 2095420)
