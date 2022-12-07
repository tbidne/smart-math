module Test.Algebra.Additive.AGroup (props) where

import Equality (Equality (..))
import Gens qualified
import Hedgehog (Gen, PropertyName, (===))
import Hedgehog qualified as H
import Numeric.Algebra.Additive.AGroup (AGroup (..))
import Numeric.Algebra.Additive.AMonoid (AMonoid (..))
import Test.Tasty (TestName, TestTree)
import Test.Tasty qualified as T
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Additive Group"
    [ subProps,
      subIdentProps
    ]

subProps :: TestTree
subProps =
  T.testGroup
    "(.-.) === (-)"
    [ fractionSub
    ]

fractionSub :: TestTree
fractionSub = agroupSubEq Gens.fraction MkEqExact "Fraction" "fractionSub"

subIdentProps :: TestTree
subIdentProps =
  T.testGroup
    "Subtraction is the inverse: zero == x .-. x"
    [ fractionSubIdent,
      modNSubIdent,
      modPSubIdent
    ]

fractionSubIdent :: TestTree
fractionSubIdent = agroupSubIdent Gens.fraction "Fraction" "fractionSubIdent"

modNSubIdent :: TestTree
modNSubIdent = agroupSubIdent Gens.modN "ModN" "modNSubIdent"

modPSubIdent :: TestTree
modPSubIdent = agroupSubIdent Gens.modP "ModP" "modPSubIdent"

agroupSubEq ::
  ( AGroup a,
    Num a,
    Show a
  ) =>
  Gen a ->
  (a -> Equality eq a) ->
  TestName ->
  PropertyName ->
  TestTree
agroupSubEq = Utils.binaryEq (-) (.-.)

agroupSubIdent ::
  ( AGroup a,
    Eq a,
    Show a
  ) =>
  Gen a ->
  TestName ->
  PropertyName ->
  TestTree
agroupSubIdent gen desc propName =
  Utils.testPropertyCompat desc propName $
    H.property $ do
      x <- H.forAll gen
      zero === x .-. x
