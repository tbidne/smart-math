{-# LANGUAGE CPP #-}

module Test.Prelude
  ( module X,
    testPropertyCompat,
  )
where

import Data.Int as X (Int16, Int32, Int64, Int8)
import Data.Proxy as X (Proxy (Proxy))
import Data.Typeable as X (Typeable)
import Data.Word as X (Word16, Word32, Word64, Word8)
import GHC.Natural as X (Natural)
import GHC.TypeNats as X (KnownNat, natVal)
import Hedgehog as X
  ( Gen,
    Property,
    PropertyName,
    Range,
    annotate,
    annotateShow,
    assert,
    diff,
    failure,
    forAll,
    property,
    (===),
  )
import Optics.Core as X (view, (%), _1, _2)
import Test.Tasty as X (TestName, TestTree, testGroup)
import Test.Tasty.HUnit as X
  ( assertBool,
    assertFailure,
    testCase,
    (@=?),
  )
import Test.Tasty.Hedgehog qualified as TH

testPropertyCompat :: TestName -> PropertyName -> Property -> TestTree
#if MIN_VERSION_tasty_hedgehog(1, 2, 0)
testPropertyCompat = TH.testPropertyNamed
#else
testPropertyCompat tn _ = TH.testProperty tn
#endif
