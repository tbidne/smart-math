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
    testPropertyCompat,
  )
where

import Equality (Equality)
import Hedgehog (Gen, Property, PropertyName, (===))
import Hedgehog qualified as H
import MaxRuns (MaxRuns (..))
import Test.Tasty (TestName, TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH

binaryEq ::
  Show a =>
  (a -> a -> a) ->
  (a -> a -> a) ->
  Gen a ->
  (a -> Equality eq a) ->
  TestName ->
  PropertyName ->
  TestTree
binaryEq expectedFn actualFn gen equalityCons desc propName = T.askOption $ \(MkMaxRuns limit) ->
  testPropertyCompat desc propName $
    H.withTests limit $
      H.property $ do
        x <- H.forAll gen
        y <- H.forAll gen
        let actual = actualFn x y
            expected = expectedFn x y
        equalityCons expected === equalityCons actual

associativity :: (Eq a, Show a) => (a -> a -> a) -> Gen a -> TestName -> PropertyName -> TestTree
associativity f gen desc propName = T.askOption $ \(MkMaxRuns limit) ->
  testPropertyCompat desc propName $
    H.withTests limit $
      H.property $ do
        x <- H.forAll gen
        y <- H.forAll gen
        z <- H.forAll gen
        let lhsPreSum = y `f` z
            lhs = x `f` lhsPreSum
            rhsPreSum = x `f` y
            rhs = rhsPreSum `f` z
        H.annotateShow lhsPreSum
        H.annotateShow lhs
        H.annotateShow rhsPreSum
        H.annotateShow rhs
        -- x `f` (y `f` z) === (x `f` y) `f` z,
        -- but with more granular logging
        lhs === rhs

identity :: Show a => (a -> a -> a) -> a -> Gen a -> (a -> Equality eq a) -> TestName -> PropertyName -> TestTree
identity f ident gen eqCons desc propName = T.askOption $ \(MkMaxRuns limit) ->
  testPropertyCompat desc propName $
    H.withTests limit $
      H.property $ do
        x <- H.forAll gen
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

testPropertyCompat :: TestName -> PropertyName -> Property -> TestTree
#if MIN_VERSION_tasty_hedgehog(1, 2, 0)
testPropertyCompat = TH.testPropertyNamed
#else
testPropertyCompat tn _ = TH.testProperty tn
#endif
