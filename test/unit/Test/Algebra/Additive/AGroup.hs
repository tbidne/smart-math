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
    [ afractionSub,
      bfractionSub
    ]

afractionSub :: TestTree
afractionSub = agroupSubEq Gens.afraction MkEqExact "Fraction.Algebra" "afractionSub"

bfractionSub :: TestTree
bfractionSub = agroupSubEq Gens.bfraction MkEqExact "Fraction.Base" "bfractionSub"

subIdentProps :: TestTree
subIdentProps =
  testGroup
    "Subtraction is the inverse: zero == x .-. x"
    [ afractionSubIdent,
      bfractionSubIdent,
      amodNSubIdent,
      bmodNSubIdent,
      amodPSubIdent,
      bmodPSubIdent
    ]

afractionSubIdent :: TestTree
afractionSubIdent = agroupSubIdent Gens.afraction "Fraction.Algebra" "afractionSubIdent"

bfractionSubIdent :: TestTree
bfractionSubIdent = agroupSubIdent Gens.bfraction "Fraction.Base" "bfractionSubIdent"

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
