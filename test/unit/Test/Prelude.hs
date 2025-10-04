{-# LANGUAGE CPP #-}

module Test.Prelude
  ( module X,
    assertLeftHH,
    assertLeftHU,
    testPropertyCompat,
  )
where

import Data.Int as X (Int16, Int32, Int64, Int8)
import Data.Proxy as X (Proxy (Proxy))
import Data.Text qualified as T
import Data.Typeable as X (Typeable)
import Data.Word as X (Word16, Word32, Word64, Word8)
import GHC.Natural as X (Natural)
import GHC.TypeNats as X (KnownNat, natVal)
import Hedgehog as X
  ( Gen,
    Property,
    PropertyName,
    PropertyT,
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

assertLeftHH :: (Show a) => String -> Either String a -> PropertyT IO ()
assertLeftHH _ (Right x) = do
  annotate $ "Expected Left, received Right: " ++ show x
  failure
assertLeftHH pfx (Left msg) = do
  annotate pfx
  annotate msg
  assert $ (T.pack pfx) `T.isPrefixOf` (T.pack msg)

assertLeftHU :: (Show a) => String -> Either String a -> IO ()
assertLeftHU _ (Right x) = assertFailure $ "Expected Left, received Right: " ++ show x
assertLeftHU pfx (Left msg) = do
  let err =
        mconcat
          [ "Expected '",
            pfx,
            "', received: '",
            msg,
            "'"
          ]
  assertBool err ((T.pack pfx) `T.isPrefixOf` (T.pack msg))

testPropertyCompat :: TestName -> PropertyName -> Property -> TestTree
#if MIN_VERSION_tasty_hedgehog(1, 2, 0)
testPropertyCompat = TH.testPropertyNamed
#else
testPropertyCompat tn _ = TH.testProperty tn
#endif
