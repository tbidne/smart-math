{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils
  ( -- * Equality Functions
    binaryEq,

    -- * Laws
    associativity,
    identity,

    -- * Logic
    (==>),
    (<=>),

    -- * Misc
    assertPureErrorCall,
  )
where

import Control.DeepSeq (NFData, force)
import Control.Exception
  ( ErrorCall,
    Exception (displayException),
    evaluate,
    try,
  )
import Data.List qualified as L
import Equality (Equality)
import Test.Prelude

binaryEq ::
  (Show a) =>
  (a -> a -> a) ->
  (a -> a -> a) ->
  Gen a ->
  (a -> Equality eq a) ->
  TestName ->
  PropertyName ->
  TestTree
binaryEq expectedFn actualFn gen equalityCons desc propName =
  testPropertyCompat desc propName $
    property $ do
      x <- forAll gen
      y <- forAll gen
      let actual = actualFn x y
          expected = expectedFn x y
      equalityCons expected === equalityCons actual

associativity :: (Eq a, Show a) => (a -> a -> a) -> Gen a -> TestName -> PropertyName -> TestTree
associativity f gen desc propName =
  testPropertyCompat desc propName $
    property $ do
      x <- forAll gen
      y <- forAll gen
      z <- forAll gen
      let lhsPreSum = y `f` z
          lhs = x `f` lhsPreSum
          rhsPreSum = x `f` y
          rhs = rhsPreSum `f` z
      annotateShow lhsPreSum
      annotateShow lhs
      annotateShow rhsPreSum
      annotateShow rhs
      -- x `f` (y `f` z) === (x `f` y) `f` z,
      -- but with more granular logging
      lhs === rhs

identity :: (Show a) => (a -> a -> a) -> a -> Gen a -> (a -> Equality eq a) -> TestName -> PropertyName -> TestTree
identity f ident gen eqCons desc propName =
  testPropertyCompat desc propName $
    property $ do
      x <- forAll gen
      eqCons (f x ident) === eqCons (f ident x)

(==>) :: Bool -> Bool -> Bool
True ==> False = False
_ ==> _ = True

infixr 1 ==>

(<=>) :: Bool -> Bool -> Bool
True <=> True = True
False <=> False = True
_ <=> _ = False

infixr 1 <=>

assertPureErrorCall :: (NFData a, Show a) => String -> a -> IO ()
assertPureErrorCall = assertPureException @ErrorCall

assertPureException ::
  forall e a.
  ( Exception e,
    NFData a,
    Show a
  ) =>
  String ->
  a ->
  IO ()
assertPureException expected x = do
  eResult <- try @e $ evaluate $ force x
  case eResult of
    Left ex -> do
      let msg =
            mconcat
              [ "Expected\n\n'",
                expected,
                "'\n\nto be a prefix of\n\n'",
                displayException ex,
                "'."
              ]
      assertBool msg $ expected `L.isPrefixOf` displayException ex
    Right r -> assertFailure $ "Expected exception, received: " ++ show r
