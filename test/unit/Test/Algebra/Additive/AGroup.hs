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
fractionSub = agroupSubEq Gens.fraction MkEqExact "Fraction" "fractionSub"

subIdentProps :: TestTree
subIdentProps =
  testGroup
    "Subtraction is the inverse: zero == x .-. x"
    [ fractionSubIdent,
      amodNSubIdent,
      bmodNSubIdent,
      amodPSubIdent,
      bmodPSubIdent
    ]

fractionSubIdent :: TestTree
fractionSubIdent = agroupSubIdent Gens.fraction "Fraction" "fractionSubIdent"

amodNSubIdent :: TestTree
amodNSubIdent = agroupSubIdent Gens.amodN "ModN.Algebra" "amodNSubIdent"

bmodNSubIdent :: TestTree
bmodNSubIdent = agroupSubIdent Gens.bmodN "ModN.Base" "bmodNSubIdent"

amodPSubIdent :: TestTree
amodPSubIdent = agroupSubIdent Gens.amodP "ModP.Algebra" "amodPSubIdent"

bmodPSubIdent :: TestTree
bmodPSubIdent = agroupSubIdent Gens.bmodP "ModP.Base" "bmodPSubIdent"

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
  testPropertyCompat desc propName $
    property $ do
      x <- forAll gen
      zero === x .-. x
