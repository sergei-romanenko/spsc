module Algebra_Tests where

import Test.HUnit
import qualified Data.Map as Map

import SLanguage
import Algebra
import SParsers

algebraTests = TestLabel "Algebra" ( TestList [
  testVars,
  testMatchV_E, testMatchC_V, testMatchC_C, testMatchC1_C2,
  testMatchC_F, testMatchX_X_Eq, testMatchX_X_Ne,
  testEquiv_yes, testEquiv_no
  ] )

testVars = TestCase $ assertEqual
  "Vars" ["x", "z"] (vars $ pExp "gAdd(gAdd(x,Z),S(z))")

showMatchRes Nothing = "*"
showMatchRes (Just subst) =
  concat[ vname ++ "->" ++ show e ++ ";" | (vname, e) <- Map.toAscList subst ]

testMatch label alpha beta expected = TestCase $ assertEqual
  label expected (showMatchRes (matchAgainst (pExp alpha) (pExp beta)))

testMatchV_E = testMatch "MatchV_E" "x" "S(Z)" "x->S(Z);"
testMatchC_V = testMatch "testMatchC_V" "Z" "x" "*"
testMatchC_C = testMatch "testMatchC_C" "C(x,y)" "C(A,B)" "x->A;y->B;"
testMatchC1_C2 = testMatch "testMatchC1_C2" "C(x,y)" "D(A,B)" "*"
testMatchC_F = testMatch "testMatchC_F" "C(x,y)" "f(A,B)" "*"
testMatchX_X_Eq = testMatch "testMatchC_C_Eq" "C(x,x)" "C(A,A)" "x->A;"
testMatchX_X_Ne = testMatch "testMatchC_C_Ne" "C(x,x)" "C(A,B)" "*"

testEquiv label e1 e2 expected = TestCase $ assertEqual
  label expected ((pExp e1) `equiv` (pExp e2))

testEquiv_yes = testEquiv "testEquiv_yes"
  "gA(fB(x,y),C)" "gA(fB(a,b),C)" True

testEquiv_no = testEquiv "testEquiv_yes"
  "gA(fB(x,y),x)" "gA(fB(a,a),b)" False
