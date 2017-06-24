module Supercompiler_Tests where

import Control.Monad.State
import Test.HUnit

import SLanguage
import SParsers
import Supercompiler

drStepTests = TestLabel "DrStep" ( TestList [
  testCtr, testFCall, testGCallCtr, testGCallVar, testGCallGeneral, testLet
  ] )

testDrStep prog label e expected =
  testDrStep' (pProg prog) label (pExp e) expected

testDrStep' prog label e expected =
  TestCase $ assertEqual
    label expected (show $ runDrStep prog e)

runDrStep prog e =
  (evalState $ prog `drivingStep` e) 100

testBuildPrTree prog e =
  basic_buildProcessTree (pProg prog) (pExp e)

buildStart buildStep prog tree =
  case unprocessedNodes tree of
    [] -> return tree
    beta : _ ->
      do tree' <- buildStep prog tree beta
         return tree'

buildPrTree1 prog e =
  (evalState $ buildStart basic_buildStep prog (initTree e)) 10000 

testBuildPrTree1 prog e =
  buildPrTree1 (pProg prog) (pExp e)

buildPrTree1Adv prog e =
  (evalState $ buildStart advanced_buildStep prog (initTree e)) 10000 

testBPT1Adv prog e =
  buildPrTree1Adv (pProg prog) (pExp e)

pAdd = "gAdd(Z,y)=y;gAdd(S(x),y)=S(gAdd(x,y));"
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
  "[(b,Just a = Z),(gAddAcc(v100,S(b)),Just a = S(v100))]"

testGCallGeneral = testDrStep
  pAddAcc
  "GCallGeneral"
  "gAddAcc(gAddAcc(a, b), c)"
  "[(gAddAcc(b,c),Just a = Z),(gAddAcc(gAddAcc(v100,S(b)),c),Just a = S(v100))]"

testLet = testDrStep'
  (Program [])
  "Let"
  (Let (Call Ctr "C" [Var "x", Var "y"]) [("x", Var "a"), ("y", Var "b")])
  "[(C(x,y),Nothing),(a,Nothing),(b,Nothing)]"

testPrTrVar = testBuildPrTree "" "x"
testPrTrCtr = testBuildPrTree "" "S(Z)"
testAdd1_0 = testBuildPrTree pAddAcc "gAddAcc(S(Z), Z)"
testAddAB = testBuildPrTree pAdd "gAdd(a, b)"
testAddAdd = testBuildPrTree pAdd "gAdd(gAdd(a,b),c)"

testAPTVar = testBPT1Adv "" "x"
testAPTCtr = testBPT1Adv "" "S(Z)"
testAAdd1_0 = testBPT1Adv pAddAcc "gAddAcc(S(Z), Z)"
testAAddAB = testBPT1Adv pAdd "gAdd(a, b)"
testAAddAdd = testBPT1Adv pAdd "gAdd(gAdd(a,b),c)"
