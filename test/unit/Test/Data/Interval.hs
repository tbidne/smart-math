{-# LANGUAGE TemplateHaskell #-}

-- HLINT incorrectly thinks TH is not needed

{- HLINT ignore "Unused LANGUAGE pragma" -}

module Test.Data.Interval (tests) where

import Hedgehog (Gen, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import Numeric.Data.Interval (Interval, IntervalBound (Closed, None, Open))
import Numeric.Data.Interval qualified as Interval
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.HUnit ((@=?))
import Test.Tasty.HUnit qualified as HUnit
import Utils qualified

tests :: TestTree
tests =
  T.testGroup
    "Numeric.Data.Interval"
    [ props,
      specs
    ]

props :: TestTree
props =
  T.testGroup
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
  Utils.testPropertyCompat desc "mkIntervalSucceedsUnbounded" $
    H.property $ do
      x <- H.forAll genUnbounded

      let i = Interval.mkInterval @None @None x
      H.annotateShow i

      Just x === fmap Interval.unInterval i
  where
    desc = "x in [0, \8734) succeeds Interval (-\8734, \8734)"

mkIntervalSucceedsOpenLowerBounded :: TestTree
mkIntervalSucceedsOpenLowerBounded =
  Utils.testPropertyCompat desc "mkIntervalSucceedsOpenLowerBounded" $
    H.property $ do
      x <- H.forAll (genWithOpenLowerBound 5)

      let i = Interval.mkInterval @(Open 5) @None x
      H.annotateShow i

      Just x === fmap Interval.unInterval i
  where
    desc = "x in (5, \8734) succeeds Interval (5, \8734)"

mkIntervalFailsOpenLowerBounded :: TestTree
mkIntervalFailsOpenLowerBounded =
  Utils.testPropertyCompat desc "mkIntervalFailsOpenLowerBounded" $
    H.property $ do
      x <- H.forAll (genWithClosedUpperBound 20)

      let i = Interval.mkInterval @(Open 20) @None x
      H.annotateShow i

      Nothing === fmap Interval.unInterval i
  where
    desc = "x in [0, 20) fails Interval (20, \8734)"

mkIntervalSucceedsClosedLowerBounded :: TestTree
mkIntervalSucceedsClosedLowerBounded =
  Utils.testPropertyCompat desc "mkIntervalSucceedsClosedLowerBounded" $
    H.property $ do
      x <- H.forAll (genWithClosedLowerBound 5)

      let i = Interval.mkInterval @(Closed 5) @None x
      H.annotateShow i

      Just x === fmap Interval.unInterval i
  where
    desc = "x in [5, \8734) succeeds Interval [5, \8734)"

mkIntervalFailsClosedLowerBounded :: TestTree
mkIntervalFailsClosedLowerBounded =
  Utils.testPropertyCompat desc "mkIntervalFailsClosedLowerBounded" $
    H.property $ do
      x <- H.forAll (genWithOpenUpperBound 20)

      let i = Interval.mkInterval @(Closed 20) @None x
      H.annotateShow i

      Nothing === fmap Interval.unInterval i
  where
    desc = "x in [0, 20) fails Interval [20, \8734)"

mkIntervalSucceedsOpenUpperBounded :: TestTree
mkIntervalSucceedsOpenUpperBounded =
  Utils.testPropertyCompat desc "mkIntervalSucceedsOpenUpperBounded" $
    H.property $ do
      x <- H.forAll (genWithOpenUpperBound 50)

      let i = Interval.mkInterval @None @(Open 50) x
      H.annotateShow i

      Just x === fmap Interval.unInterval i
  where
    desc = "x in [0, 50) succeeds Interval (-\8734, 50)"

mkIntervalFailsOpenUpperBounded :: TestTree
mkIntervalFailsOpenUpperBounded =
  Utils.testPropertyCompat desc "mkIntervalFailsOpenUpperBounded" $
    H.property $ do
      x <- H.forAll (genWithClosedLowerBound 50)

      let i = Interval.mkInterval @None @(Open 50) x
      H.annotateShow i

      Nothing === fmap Interval.unInterval i
  where
    desc = "x in [50, \8734) fails Interval (-\8734, 50)"

mkIntervalSucceedsClosedUpperBounded :: TestTree
mkIntervalSucceedsClosedUpperBounded =
  Utils.testPropertyCompat desc "mkIntervalSucceedsClosedUpperBounded" $
    H.property $ do
      x <- H.forAll (genWithClosedUpperBound 50)

      let i = Interval.mkInterval @None @(Closed 50) x
      H.annotateShow i

      Just x === fmap Interval.unInterval i
  where
    desc = "x in [0, 50] succeeds Interval (-\8734, 50]"

mkIntervalFailsClosedUpperBounded :: TestTree
mkIntervalFailsClosedUpperBounded =
  Utils.testPropertyCompat desc "mkIntervalFailsClosedUpperBounded" $
    H.property $ do
      x <- H.forAll (genWithOpenLowerBound 50)

      let i = Interval.mkInterval @None @(Closed 50) x
      H.annotateShow i

      Nothing === fmap Interval.unInterval i
  where
    desc = "x in (50, \8734) fails Interval (-\8734, 50]"

specs :: TestTree
specs =
  T.testGroup
    "Specs"
    [ lowerBoundedOpenTests,
      lowerBoundedClosedTests,
      upperBoundedOpenTests,
      upperBoundedClosedTests
    ]
  where
    lowerBoundedOpenTests =
      T.testGroup
        "Open lower bound"
        [ HUnit.testCase "6 passes (5, \8734)" $ do
            let expected :: Interval (Open 5) None Int
                expected = $$(Interval.mkIntervalTH 6)
            Just expected @=? mkLowerBoundedOpen 6,
          HUnit.testCase "5 fails (5, \8734)" $ do
            Nothing @=? mkLowerBoundedOpen 5,
          HUnit.testCase "4 fails (5, \8734)" $ do
            Nothing @=? mkLowerBoundedOpen 4
        ]

    mkLowerBoundedOpen :: Int -> Maybe (Interval (Open 5) None Int)
    mkLowerBoundedOpen = Interval.mkInterval

    lowerBoundedClosedTests =
      T.testGroup
        "Closed lower bound"
        [ HUnit.testCase "6 passes [5, \8734)" $ do
            let expected :: Interval (Closed 5) None Int
                expected = $$(Interval.mkIntervalTH 6)
            Just expected @=? mkLowerBoundedClosed 6,
          HUnit.testCase "5 passes [5, \8734)" $ do
            let expected :: Interval (Closed 5) None Int
                expected = $$(Interval.mkIntervalTH 5)
            Just expected @=? mkLowerBoundedClosed 5,
          HUnit.testCase "4 fails [5, \8734)" $ do
            Nothing @=? mkLowerBoundedClosed 4
        ]

    mkLowerBoundedClosed :: Int -> Maybe (Interval (Closed 5) None Int)
    mkLowerBoundedClosed = Interval.mkInterval

    upperBoundedOpenTests =
      T.testGroup
        "Open upper bound"
        [ HUnit.testCase "6 passes (-\8734, 50)" $ do
            let expected :: Interval None (Open 50) Int
                expected = $$(Interval.mkIntervalTH 6)
            Just expected @=? mkUpperBoundedOpen 6,
          HUnit.testCase "50 fails (-\8734, 50)" $ do
            Nothing @=? mkUpperBoundedOpen 50,
          HUnit.testCase "55 fails (-\8734, 50)" $ do
            Nothing @=? mkUpperBoundedOpen 55
        ]

    mkUpperBoundedOpen :: Int -> Maybe (Interval None (Open 50) Int)
    mkUpperBoundedOpen = Interval.mkInterval

    upperBoundedClosedTests =
      T.testGroup
        "Open upper bound"
        [ HUnit.testCase "6 passes (-\8734, 50]" $ do
            let expected :: Interval None (Closed 50) Int
                expected = $$(Interval.mkIntervalTH 6)
            Just expected @=? mkUpperBoundedClosed 6,
          HUnit.testCase "50 passes (-\8734, 50]" $ do
            let expected :: Interval None (Closed 50) Int
                expected = $$(Interval.mkIntervalTH 50)
            Just expected @=? mkUpperBoundedClosed 50,
          HUnit.testCase "55 fails (-\8734, 50]" $ do
            Nothing @=? mkUpperBoundedClosed 55
        ]

    mkUpperBoundedClosed :: Int -> Maybe (Interval None (Closed 50) Int)
    mkUpperBoundedClosed = Interval.mkInterval

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
