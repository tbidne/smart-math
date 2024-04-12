{-# LANGUAGE TemplateHaskell #-}

-- HLINT incorrectly thinks TH is not needed

{- HLINT ignore "Unused LANGUAGE pragma" -}

module Test.Data.Interval (tests) where

import Data.Text.Display qualified as D
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import Numeric.Data.Interval qualified as Interval
import Numeric.Data.Interval.Internal
  ( Interval (UnsafeInterval),
    IntervalBound (Closed, None, Open),
  )
import Test.Prelude
import Utils qualified

tests :: TestTree
tests =
  testGroup
    "Numeric.Data.Interval"
    [ props,
      specs
    ]

props :: TestTree
props =
  testGroup
    "Properties"
    [ mkIntervalSucceedsUnbounded,
      mkIntervalSucceedsOpenLowerBounded,
      mkIntervalFailsOpenLowerBounded,
      mkIntervalSucceedsClosedLowerBounded,
      mkIntervalFailsClosedLowerBounded,
      mkIntervalSucceedsOpenUpperBounded,
      mkIntervalFailsOpenUpperBounded,
      mkIntervalSucceedsClosedUpperBounded,
      mkIntervalFailsClosedUpperBounded
    ]

mkIntervalSucceedsUnbounded :: TestTree
mkIntervalSucceedsUnbounded =
  testPropertyCompat desc "mkIntervalSucceedsUnbounded" $
    property $ do
      x <- forAll genUnbounded

      let i = Interval.mkInterval @None @None x
      annotateShow i

      Just x === fmap Interval.unInterval i
  where
    desc = "x in [0, \8734) succeeds Interval (-\8734, \8734)"

mkIntervalSucceedsOpenLowerBounded :: TestTree
mkIntervalSucceedsOpenLowerBounded =
  testPropertyCompat desc "mkIntervalSucceedsOpenLowerBounded" $
    property $ do
      x <- forAll (genWithOpenLowerBound 5)

      let i = Interval.mkInterval @(Open 5) @None x
      annotateShow i

      Just x === fmap Interval.unInterval i
  where
    desc = "x in (5, \8734) succeeds Interval (5, \8734)"

mkIntervalFailsOpenLowerBounded :: TestTree
mkIntervalFailsOpenLowerBounded =
  testPropertyCompat desc "mkIntervalFailsOpenLowerBounded" $
    property $ do
      x <- forAll (genWithClosedUpperBound 20)

      let i = Interval.mkInterval @(Open 20) @None x
      annotateShow i

      Nothing === fmap Interval.unInterval i
  where
    desc = "x in [0, 20) fails Interval (20, \8734)"

mkIntervalSucceedsClosedLowerBounded :: TestTree
mkIntervalSucceedsClosedLowerBounded =
  testPropertyCompat desc "mkIntervalSucceedsClosedLowerBounded" $
    property $ do
      x <- forAll (genWithClosedLowerBound 5)

      let i = Interval.mkInterval @(Closed 5) @None x
      annotateShow i

      Just x === fmap Interval.unInterval i
  where
    desc = "x in [5, \8734) succeeds Interval [5, \8734)"

mkIntervalFailsClosedLowerBounded :: TestTree
mkIntervalFailsClosedLowerBounded =
  testPropertyCompat desc "mkIntervalFailsClosedLowerBounded" $
    property $ do
      x <- forAll (genWithOpenUpperBound 20)

      let i = Interval.mkInterval @(Closed 20) @None x
      annotateShow i

      Nothing === fmap Interval.unInterval i
  where
    desc = "x in [0, 20) fails Interval [20, \8734)"

mkIntervalSucceedsOpenUpperBounded :: TestTree
mkIntervalSucceedsOpenUpperBounded =
  testPropertyCompat desc "mkIntervalSucceedsOpenUpperBounded" $
    property $ do
      x <- forAll (genWithOpenUpperBound 50)

      let i = Interval.mkInterval @None @(Open 50) x
      annotateShow i

      Just x === fmap Interval.unInterval i
  where
    desc = "x in [0, 50) succeeds Interval (-\8734, 50)"

mkIntervalFailsOpenUpperBounded :: TestTree
mkIntervalFailsOpenUpperBounded =
  testPropertyCompat desc "mkIntervalFailsOpenUpperBounded" $
    property $ do
      x <- forAll (genWithClosedLowerBound 50)

      let i = Interval.mkInterval @None @(Open 50) x
      annotateShow i

      Nothing === fmap Interval.unInterval i
  where
    desc = "x in [50, \8734) fails Interval (-\8734, 50)"

mkIntervalSucceedsClosedUpperBounded :: TestTree
mkIntervalSucceedsClosedUpperBounded =
  testPropertyCompat desc "mkIntervalSucceedsClosedUpperBounded" $
    property $ do
      x <- forAll (genWithClosedUpperBound 50)

      let i = Interval.mkInterval @None @(Closed 50) x
      annotateShow i

      Just x === fmap Interval.unInterval i
  where
    desc = "x in [0, 50] succeeds Interval (-\8734, 50]"

mkIntervalFailsClosedUpperBounded :: TestTree
mkIntervalFailsClosedUpperBounded =
  testPropertyCompat desc "mkIntervalFailsClosedUpperBounded" $
    property $ do
      x <- forAll (genWithOpenLowerBound 50)

      let i = Interval.mkInterval @None @(Closed 50) x
      annotateShow i

      Nothing === fmap Interval.unInterval i
  where
    desc = "x in (50, \8734) fails Interval (-\8734, 50]"

elimProps :: TestTree
elimProps =
  testPropertyCompat desc "elimProps" $
    property $ do
      b@(UnsafeInterval x) <- forAll genUnboundedInterval

      x === Interval.unInterval b
      x === b.unInterval
      x === view #unInterval b
      x === view Interval._MkInterval b
  where
    desc = "elim (UnsafeInterval x) === x"

specs :: TestTree
specs =
  testGroup
    "Specs"
    [ lowerBoundedOpenTests,
      lowerBoundedClosedTests,
      upperBoundedOpenTests,
      upperBoundedClosedTests,
      testUnsafe,
      showSpecs,
      displaySpecs
    ]
  where
    lowerBoundedOpenTests =
      testGroup
        "Open lower bound"
        [ testCase "6 passes (5, \8734)" $ do
            let expected :: Interval (Open 5) None Int
                expected = $$(Interval.mkIntervalTH 6)
            Just expected @=? mkLowerBoundedOpen 6,
          testCase "5 fails (5, \8734)" $ do
            Nothing @=? mkLowerBoundedOpen 5,
          testCase "4 fails (5, \8734)" $ do
            Nothing @=? mkLowerBoundedOpen 4
        ]

    mkLowerBoundedOpen :: Int -> Maybe (Interval (Open 5) None Int)
    mkLowerBoundedOpen = Interval.mkInterval

    lowerBoundedClosedTests =
      testGroup
        "Closed lower bound"
        [ testCase "6 passes [5, \8734)" $ do
            let expected :: Interval (Closed 5) None Int
                expected = $$(Interval.mkIntervalTH 6)
            Just expected @=? mkLowerBoundedClosed 6,
          testCase "5 passes [5, \8734)" $ do
            let expected :: Interval (Closed 5) None Int
                expected = $$(Interval.mkIntervalTH 5)
            Just expected @=? mkLowerBoundedClosed 5,
          testCase "4 fails [5, \8734)" $ do
            Nothing @=? mkLowerBoundedClosed 4
        ]

    mkLowerBoundedClosed :: Int -> Maybe (Interval (Closed 5) None Int)
    mkLowerBoundedClosed = Interval.mkInterval

    upperBoundedOpenTests =
      testGroup
        "Open upper bound"
        [ testCase "6 passes (-\8734, 50)" $ do
            let expected :: Interval None (Open 50) Int
                expected = $$(Interval.mkIntervalTH 6)
            Just expected @=? mkUpperBoundedOpen 6,
          testCase "50 fails (-\8734, 50)" $ do
            Nothing @=? mkUpperBoundedOpen 50,
          testCase "55 fails (-\8734, 50)" $ do
            Nothing @=? mkUpperBoundedOpen 55
        ]

    mkUpperBoundedOpen :: Int -> Maybe (Interval None (Open 50) Int)
    mkUpperBoundedOpen = Interval.mkInterval

    upperBoundedClosedTests =
      testGroup
        "Open upper bound"
        [ testCase "6 passes (-\8734, 50]" $ do
            let expected :: Interval None (Closed 50) Int
                expected = $$(Interval.mkIntervalTH 6)
            Just expected @=? mkUpperBoundedClosed 6,
          testCase "50 passes (-\8734, 50]" $ do
            let expected :: Interval None (Closed 50) Int
                expected = $$(Interval.mkIntervalTH 50)
            Just expected @=? mkUpperBoundedClosed 50,
          testCase "55 fails (-\8734, 50]" $ do
            Nothing @=? mkUpperBoundedClosed 55
        ]

    mkUpperBoundedClosed :: Int -> Maybe (Interval None (Closed 50) Int)
    mkUpperBoundedClosed = Interval.mkInterval

genUnboundedInterval :: Gen (Interval None None Int)
genUnboundedInterval = Interval.unsafeInterval <$> genUnbounded

genUnbounded :: Gen Int
genUnbounded = HG.integral $ HR.exponentialFrom 0 0 1_000_000_000

genWithOpenLowerBound :: Int -> Gen Int
genWithOpenLowerBound n = HG.integral $ HR.exponential (n + 1) 1_000_000_000

genWithClosedLowerBound :: Int -> Gen Int
genWithClosedLowerBound n = HG.integral $ HR.exponential n 1_000_000_000

genWithOpenUpperBound :: Int -> Gen Int
genWithOpenUpperBound = HG.integral . HR.exponential 0 . (\x -> x - 1)

genWithClosedUpperBound :: Int -> Gen Int
genWithClosedUpperBound = HG.integral . HR.exponential 0

testUnsafe :: TestTree
testUnsafe = testCase "Test unsafeInterval" $ do
  UnsafeInterval 5 @=? Interval.unsafeInterval @(Open 1) @None @Integer 5

  Utils.assertPureErrorCall expectedEx (Interval.unsafeInterval @(Open 1) @None @Integer 1)
  where
    expectedEx = "Numeric.Data.Interval.unsafeInterval: Wanted value in (1, ∞), received: 1"

showSpecs :: TestTree
showSpecs = testCase "Shows intervals" $ do
  "UnsafeInterval None None 2" @=? show (Interval.unsafeInterval @None @None @Integer 2)
  "UnsafeInterval (Open 1) (Closed 10) 7" @=? show (Interval.unsafeInterval @(Open 1) @(Closed 10) @Integer 7)
  "UnsafeInterval (Closed 1) (Open 10) 7" @=? show (Interval.unsafeInterval @(Closed 1) @(Open 10) @Integer 7)

displaySpecs :: TestTree
displaySpecs = testCase "Displays intervals" $ do
  "2 \8712 (-\8734, \8734)" @=? D.display (Interval.unsafeInterval @None @None @Integer 2)
  "7 \8712 (1, 10]" @=? D.display (Interval.unsafeInterval @(Open 1) @(Closed 10) @Integer 7)
  "7 \8712 [1, 10)" @=? D.display (Interval.unsafeInterval @(Closed 1) @(Open 10) @Integer 7)
