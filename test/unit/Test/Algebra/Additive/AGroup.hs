module Test.Algebra.Additive.AGroup (props) where

import Equality (Equality (MkEqExact))
import Gens qualified
import Numeric.Algebra.Additive.AGroup (AGroup ((.-.)))
import Numeric.Algebra.Additive.AMonoid (AMonoid (zero))
import Test.Prelude
import Utils qualified

props :: TestTree
props =
  testGroup
    "Additive Group"
    [ subProps,
      subIdentProps
    ]

subProps :: TestTree
subProps =
  testGroup
    "(.-.) === (-)"
    [ fractionSub
    ]

fractionSub :: TestTree
fractionSub = groupSubEq Gens.fraction MkEqExact "Fraction" "fractionSub"

subIdentProps :: TestTree
subIdentProps =
  testGroup
    "Subtraction is the inverse: zero == x .-. x"
    [ fractionSubIdent,
      fractionSubIdent,
      modNSubIdent,
      modPSubIdent
    ]

fractionSubIdent :: TestTree
fractionSubIdent = groupSubIdent Gens.fraction "Fraction" "fractionSubIdent"

modNSubIdent :: TestTree
modNSubIdent = groupSubIdent Gens.modN "ModN" "modNSubIdent"

modPSubIdent :: TestTree
modPSubIdent = groupSubIdent Gens.modP "ModP" "modPSubIdent"

groupSubEq ::
  ( AGroup a,
    Num a,
    Show a
  ) =>
  Gen a ->
  (a -> Equality eq a) ->
  TestName ->
  PropertyName ->
  TestTree
groupSubEq = Utils.binaryEq (-) (.-.)

groupSubIdent ::
  ( AGroup a,
    Eq a,
    Show a
  ) =>
  Gen a ->
  TestName ->
  PropertyName ->
  TestTree
groupSubIdent gen desc propName =
  testPropertyCompat desc propName $
    property $ do
      x <- forAll gen
      zero === x .-. x
