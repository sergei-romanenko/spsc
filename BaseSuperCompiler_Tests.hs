module BaseSuperCompiler_Tests where

import Control.Monad.State
import Test.HUnit

import SLanguage
import SParsers
import BaseSuperCompiler

testDrStep prog label e expected =
  testDrStep' (pProg prog) label (pExp e) expected

testDrStep' prog label e expected =
  TestCase $ assertEqual
    label expected (show $ runDrStep prog e)

runDrStep prog e =
  (evalState $ prog `drivingStep` e) 100

drStepTests = TestLabel "DrStep" ( TestList [
  testCtr, testFCall, testGCallCtr, testGCallVar, testGCallGeneral, testLet
  ] )

pAddAcc = "gAddAcc(Z,y)=y;gAddAcc(S(x),y)=gAddAcc(x,S(y));"

testCtr = testDrStep
  ""
  "Ctr"
  "C(a,b)"
  "[(a,Nothing),(b,Nothing)]"

testFCall = testDrStep
  "f(x)=x;"
  "FCall"
  "f(A(z))"
  "[(A(z),Nothing)]"

testGCallCtr = testDrStep
  pAddAcc
  "GCallCtr"
  "gAddAcc(S(S(Z)), Z)"
  "[(gAddAcc(S(Z),S(Z)),Nothing)]"

testGCallVar = testDrStep
  pAddAcc
  "GCallVar"
  "gAddAcc(a,b)"
  "[(b,Just a = Z),(gAddAcc($100,S(b)),Just a = S($100))]"

testGCallGeneral = testDrStep
  pAddAcc
  "GCallGeneral"
  "gAddAcc(gAddAcc(a, b), c)"
  "[(gAddAcc(b,c),Just a = Z),(gAddAcc(gAddAcc($100,S(b)),c),Just a = S($100))]"

testLet = testDrStep'
  (Program [])
  "Let"
  (Let (Call Ctr "C" [Var "x", Var "y"]) [("x", Var "a"), ("y", Var "b")])
  "[(C(x,y),Nothing),(a,Nothing),(b,Nothing)]"