module SupercompilerTests

import Data.SortedMap
import Control.Monad.State
import Test.Unit

import SLanguage
import SParsers
import ProcessTree
import Supercompiler

--

runDrStep : Program -> Exp -> List Branch
runDrStep prog e =
  (evalState $ prog `drivingStep` e) 100

evDrStep : String -> String -> Maybe String
evDrStep givenProg givenExp =
  do prog <- parseProg givenProg
     e <- parseExp givenExp
     pure $ show $ runDrStep prog e

testDrStep : String -> String -> String -> IO Bool
testDrStep prog e expected =
  assertEquals (evDrStep prog e) (Just expected)

--

evBuilder : TreeBuilder -> String -> String -> Maybe String
evBuilder treeBuilder givenProg givenExp =
  do prog <- parseProg givenProg
     e <- parseExp givenExp
     pure $ show $ treeBuilder prog e

--

buildLoop1 : BuildLoop
buildLoop1 buildStep prog tree =
  case findAnUnprocessedNode tree of
    Nothing => pure tree
    Just beta =>
      buildStep prog tree beta

basicBuilder1 : TreeBuilder
basicBuilder1 = mkTreeBuilder buildLoop1 basicBuildStep

advancedBuilder1 : TreeBuilder
advancedBuilder1 = mkTreeBuilder buildLoop1 advancedBuildStep

testBB : String -> String -> String -> IO Bool
testBB prog e expected =
  assertEquals (evBuilder basicBuilder prog e) (Just expected)

testBB1 : String -> String -> String -> IO Bool
testBB1 prog e expected =
  assertEquals (evBuilder basicBuilder1 prog e) (Just expected)

testAB1 : String -> String -> String -> IO Bool
testAB1 prog e expected =
  assertEquals (evBuilder advancedBuilder1 prog e) (Just expected)

--

pAdd : String
pAdd = "gAdd(Z,y)=y;gAdd(S(x),y)=S(gAdd(x,y));"

pAddAcc : String
pAddAcc = "gAddAcc(Z,y)=y;gAddAcc(S(x),y)=gAddAcc(x,S(y));"

testCtr : IO Bool
testCtr = testDrStep
  ""
  "C(a,b)"
  "[(a, Nothing), (b, Nothing)]"

testFCall : IO Bool
testFCall = testDrStep
  "f(x)=x;"
  "f(A(z))"
  "[(A(z), Nothing)]"

testGCallCtr : IO Bool
testGCallCtr = testDrStep
  pAddAcc
  "gAddAcc(S(S(Z)), Z)"
  "[(gAddAcc(S(Z),S(Z)), Nothing)]"

testGCallVar : IO Bool
testGCallVar = testDrStep
  pAddAcc
  "gAddAcc(a,b)"
  "[(b, Just a = Z), (gAddAcc(v100,S(b)), Just a = S(v100))]"

testGCallGeneral : IO Bool
testGCallGeneral = testDrStep
  pAddAcc
  "gAddAcc(gAddAcc(a, b), c)"
  "[(gAddAcc(b,c), Just a = Z), (gAddAcc(gAddAcc(v100,S(b)),c), Just a = S(v100))]"

testLet : IO Bool
testLet = assertEquals
  (show $ runDrStep (MkProgram [])
    (Let (Call Ctr "C" [Var "x", Var "y"]) [("x", Var "a"), ("y", Var "b")]))
  "[(C(x,y), Nothing), (a, Nothing), (b, Nothing)]"

testPrTrVar : IO Bool
testPrTrVar = testBB "" "x"
  "[{0^_: x}]"

testPrTrCtr : IO Bool
testPrTrCtr = testBB "" "S(Z)"
  "[{0^_: S(Z) @[10000]}, {10000^0: Z}]"

testAdd1_0 : IO Bool
testAdd1_0 = testBB pAddAcc "gAddAcc(S(Z), Z)"
  "[{0^_: gAddAcc(S(Z),Z) @[10000]}, {10000^0: gAddAcc(Z,S(Z)) @[10001]}, {10001^10000: S(Z) @[10002]}, {10002^10001: Z}]"

testAPTVar : IO Bool
testAPTVar = testAB1 "" "x"
  "[{0^_: x}]"

testAPTCtr : IO Bool
testAPTCtr = testAB1 "" "S(Z)"
  "[{0^_: S(Z) @[10000]}, {10000^0: Z}]"

testAAdd1_0 : IO Bool
testAAdd1_0 = testAB1 pAddAcc "gAddAcc(S(Z), Z)"
  "[{0^_: gAddAcc(S(Z),Z) @[10000]}, {10000^0: gAddAcc(Z,S(Z))}]"

testAAddAB : IO Bool
testAAddAB = testAB1 pAdd "gAdd(a, b)"
  "[{0^_: gAdd(a,b) @[10001, 10002]}, {10001^0: b ?a = Z}, {10002^0: S(gAdd(v10000,b)) ?a = S(v10000)}]"

testAAddAdd : IO Bool
testAAddAdd = testAB1 pAdd "gAdd(gAdd(a,b),c)"
  "[{0^_: gAdd(gAdd(a,b),c) @[10001, 10002]}, {10001^0: gAdd(b,c) ?a = Z}, {10002^0: gAdd(S(gAdd(v10000,b)),c) ?a = S(v10000)}]"

export
allTests : IO ()
allTests = runTests
  [ testCtr
  , testFCall
  , testGCallCtr
  , testGCallVar
  , testGCallGeneral
  , testLet

  , testPrTrVar
  , testPrTrCtr
  , testAdd1_0
  , testAPTVar
  , testAPTCtr
  , testAAdd1_0
  , testAAddAB
  , testAAddAdd
  ]
