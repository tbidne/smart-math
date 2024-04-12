module Test.Prelude
  ( module X,
  )
where

import Hedgehog as X
  ( Gen,
    Range,
    annotate,
    annotateShow,
    forAll,
    property,
    (===),
  )
import Optics.Core as X (view, (%))
import Test.Tasty as X (TestTree)
import Test.Tasty.HUnit as X (testCase, (@=?))
