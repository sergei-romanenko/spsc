module AlgebraTests

import Data.SortedMap
import Test.Unit

import SLanguage
import Algebra
import SParsers

testVars : IO Bool
testVars = assertEquals (vars <$> parseExp "gAdd(gAdd(x,Z),S(z))")
                        (Just ["x", "z"])

showMatchRes : Maybe Subst -> String
showMatchRes Nothing = "*"
showMatchRes (Just subst) =
  concat[ vname ++ "->" ++ show e ++ ";" | (vname, e) <- toList subst ]

evMatch : String -> String -> Maybe String
evMatch alpha beta =
  do e1 <- parseExp alpha
     e2 <- parseExp beta
     pure $ showMatchRes $ matchAgainst e1 e2

testMatch : String -> String -> String -> IO Bool
testMatch alpha beta expected =
  assertEquals (evMatch alpha beta) (Just expected)

testMatchV_E : IO Bool
testMatchV_E = testMatch "x" "S(Z)" "x->S(Z);"

testMatchC_V : IO Bool
testMatchC_V = testMatch "Z" "x" "*"

testMatchC_C : IO Bool
testMatchC_C = testMatch "C(x,y)" "C(A,B)" "x->A;y->B;"

testMatchC1_C2 : IO Bool
testMatchC1_C2 = testMatch "C(x,y)" "D(A,B)" "*"

testMatchC_F : IO Bool
testMatchC_F = testMatch "C(x,y)" "f(A,B)" "*"

testMatchX_X_Eq : IO Bool
testMatchX_X_Eq = testMatch "C(x,x)" "C(A,A)" "x->A;"

testMatchX_X_Ne : IO Bool
testMatchX_X_Ne = testMatch "C(x,x)" "C(A,B)" "*"

evEquiv : String -> String -> Maybe Bool
evEquiv alpha beta =
  do e1 <- parseExp alpha
     e2 <- parseExp beta
     pure (e1 `equiv` e2)

testEquiv : String -> String -> Bool -> IO Bool
testEquiv alpha beta expected =
  assertEquals (evEquiv alpha beta) (Just expected)

testEquiv_yes : IO Bool
testEquiv_yes = testEquiv "gA(fB(x,y),C)" "gA(fB(a,b),C)" True

testEquiv_no : IO Bool
testEquiv_no = testEquiv "gA(fB(x,y),x)" "gA(fB(a,a),b)" False

export
allTests : IO ()
allTests = runTests
  [testVars
  , testMatchV_E
  , testMatchC_V
  , testMatchC_C
  , testMatchC1_C2
  , testMatchC_F
  , testMatchX_X_Eq
  , testMatchX_X_Ne
  , testEquiv_yes
  , testEquiv_no]
