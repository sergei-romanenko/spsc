module PTBuilderTests

import Data.SortedMap
import Control.Monad.State
import Test.Unit

import SLanguage
import SParsers
import ProcessTree
import PTBuilder

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

buildLoopN : Nat -> BuildLoop
buildLoopN Z buildStep prog tree =
  pure tree
buildLoopN (S k) buildStep prog tree =
  case findAnUnprocessedNode tree of
    Nothing => pure tree
    Just beta =>
      do tree' <- buildStep prog tree beta
         buildLoopN k buildStep prog tree'

basicBuilderN : Nat -> TreeBuilder
basicBuilderN k = mkTreeBuilder (buildLoopN k) basicBuildStep

advancedBuilderN : Nat -> TreeBuilder
advancedBuilderN k = mkTreeBuilder (buildLoopN k) advancedBuildStep

testBB : String -> String -> String -> IO Bool
testBB prog e expected =
  assertEquals (evBuilder basicBuilder prog e) (Just expected)

testBBN : Nat -> String -> String -> String -> IO Bool
testBBN k prog e expected =
  assertEquals (evBuilder (basicBuilderN k) prog e) (Just expected)

testBB1 : String -> String -> String -> IO Bool
testBB1 = testBBN 1

testAB : String -> String -> String -> IO Bool
testAB prog e expected =
  assertEquals (evBuilder advancedBuilder prog e) (Just expected)

testABN : Nat -> String -> String -> String -> IO Bool
testABN k prog e expected =
  assertEquals (evBuilder (advancedBuilderN k) prog e) (Just expected)

testAB1 : String -> String -> String -> IO Bool
testAB1 = testABN 1

--

pAdd : String
pAdd = "gAdd(Z,y)=y;gAdd(S(x),y)=S(gAdd(x,y));"

pAddAcc : String
pAddAcc = "gAddAcc(Z,y)=y;gAddAcc(S(x),y)=gAddAcc(x,S(y));"

-- Driving

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

testFCall2 : IO Bool
testFCall2 = testDrStep
  "f(x)=f(S(x));"
  "f(a)"
  "[(f(S(a)), Nothing)]"

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

-- Basic builder

testPrTrVar : IO Bool
testPrTrVar = testBB "" "x"
  "[{0^_: x}]"

testPrTrCtr : IO Bool
testPrTrCtr = testBB "" "S(Z)"
  "[{0^_: S(Z) @[10000]}, {10000^0: Z}]"

testFromGeneral : IO Bool
testFromGeneral = testBB "f(x)=f(S(x));" "f(a)"
  "[{0^_: f(a) @[10000]}, {10000^0: let a=S(a) in f(a) @[10001, 10002]}, {10001^10000^^0: f(a)}, {10002^10000: S(a) @[10003]}, {10003^10002: a}]"

testAdd1_0 : IO Bool
testAdd1_0 = testBB pAddAcc "gAddAcc(S(Z), Z)"
  "[{0^_: gAddAcc(S(Z),Z) @[10000]}, {10000^0: gAddAcc(Z,S(Z)) @[10001]}, {10001^10000: S(Z) @[10002]}, {10002^10001: Z}]"

-- Advanced builder

testAPTVar : IO Bool
testAPTVar = testAB1 "" "x"
  "[{0^_: x}]"

testAPFCall : IO Bool
testAPFCall = testAB1 "f(x)=f(S(x));" "f(a)"
  "[{0^_: f(a) @[10000]}, {10000^0: f(S(a))}]"

testAPTCtr : IO Bool
testAPTCtr = testAB1 "" "S(Z)"
  "[{0^_: S(Z) @[10000]}, {10000^0: Z}]"

testAFromGeneral : IO Bool
testAFromGeneral = testAB "f(x)=f(S(x));" "f(a)"
  "[{0^_: f(a) @[10000]}, {10000^0: let a=S(a) in f(a) @[10001, 10002]}, {10001^10000^^0: f(a)}, {10002^10000: S(a) @[10003]}, {10003^10002: a}]"

testAFromEmb : IO Bool
testAFromEmb = testAB "f(x) = g(f(x));g(A) = B;" "f(a)"
  "[{0^_: f(a) @[10000]}, {10000^0: let v10002=f(a) in g(v10002) @[10003, 10004]}, {10003^10000: g(v10002) @[10005]}, {10004^10000^^0: f(a)}, {10005^10003: B ?v10002 = A}]"

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
  , testFCall2
  , testGCallCtr
  , testGCallVar
  , testGCallGeneral

  , testPrTrVar
  , testAPFCall
  , testPrTrCtr
  , testFromGeneral
  , testAdd1_0

  , testAPTVar
  , testAPFCall
  , testAPTCtr
  , testAFromGeneral
  , testAFromEmb
  , testAAdd1_0
  , testAAddAB
  , testAAddAdd
  ]
