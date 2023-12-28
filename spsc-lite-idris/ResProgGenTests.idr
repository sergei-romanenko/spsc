module ResProgGenTests

import Test.Unit

import SLanguage
import SParsers
import ProcessTree
import PTBuilder
import ResProgGen

-- Runners

ScpRunner : Type
ScpRunner = Program -> Exp -> (Program, Exp)

evScp : TreeBuilder -> String -> String -> Maybe String
evScp treeBuilder givenProg givenExp =
  do prog <- parseProg givenProg
     e <- parseExp givenExp
     pure $ show $ (genResidualProgram $ treeBuilder prog e)

testBS : String -> String -> String -> IO Bool
testBS prog e expected =
  assertEquals (evScp basicBuilder prog e) (Just expected)

testAS : String -> String -> String -> IO Bool
testAS prog e expected =
  assertEquals (evScp advancedBuilder prog e) (Just expected)

---- Sample programs

pAdd : String
pAdd = "gAdd(Z,y)=y;gAdd(S(x),y)=S(gAdd(x,y));"

pAddAcc : String
pAddAcc = "gAddAcc(Z,y)=y;gAddAcc(S(x),y)=gAddAcc(x,S(y));"

---- Basic supercompiler

testAddAB : IO Bool
testAddAB = testBS
  pAdd
  "gAdd(a, b)"
  "(gAdd1(a,b), gAdd1(Z,b)=b;gAdd1(S(v10000),b)=S(gAdd1(v10000,b));)"

testAddAdd : IO Bool
testAddAdd = testBS
  pAdd
  "gAdd(gAdd(a,b),c)"
  "(gAdd1(a,b,c), gAdd2(Z,c)=c;gAdd2(S(v10003),c)=S(gAdd2(v10003,c));gAdd1(Z,b,c)=gAdd2(b,c);gAdd1(S(v10000),b,c)=S(gAdd1(v10000,b,c));)"

testAddAccAB : IO Bool
testAddAccAB = testBS
  pAddAcc
  "gAddAcc(a, b)"
  "(gAddAcc1(a,b), gAddAcc1(Z,b)=b;gAddAcc1(S(v10000),b)=gAddAcc1(v10000,S(b));)"

---- Advanced supercompiler

testAdvAddAB : IO Bool
testAdvAddAB = testAS
  pAdd
  "gAdd(a, b)"
  "(gAdd1(a,b), gAdd1(Z,b)=b;gAdd1(S(v10000),b)=S(gAdd1(v10000,b));)"

testAdvAddAA : IO Bool
testAdvAddAA = testAS
  pAdd
  "gAdd(a, a)"
  "(gAdd1(a,a), gAdd1(Z,v10006)=v10006;gAdd1(S(v10010),v10006)=S(gAdd1(v10010,v10006));)"

testAdvAddAdd : IO Bool
testAdvAddAdd = testAS
  pAdd
  "gAdd(gAdd(a,b),c)"
  "(gAdd1(a,b,c), gAdd2(Z,c)=c;gAdd2(S(v10003),c)=S(gAdd2(v10003,c));gAdd1(Z,b,c)=gAdd2(b,c);gAdd1(S(v10000),b,c)=S(gAdd1(v10000,b,c));)"

testAdvAddAccAB : IO Bool
testAdvAddAccAB = testAS
  pAddAcc
  "gAddAcc(a, b)"
  "(gAddAcc1(a,b), gAddAcc1(Z,b)=b;gAddAcc1(S(v10000),b)=gAddAcc1(v10000,S(b));)"

testAdvAddAccAA : IO Bool
testAdvAddAccAA = testAS
  pAddAcc
  "gAddAcc(a, a)"
  "(gAddAcc1(a,a), gAddAcc1(Z,v10005)=v10005;gAddAcc1(S(v10009),v10005)=gAddAcc1(v10009,S(v10005));)"

testAdvAddAccAddAcc : IO Bool
testAdvAddAccAddAcc = testAS
  pAddAcc
  "gAddAcc(gAddAcc(a,b),c)"
  "(gAddAcc1(a,b,c), gAddAcc2(Z,c)=c;gAddAcc2(S(v10003),c)=gAddAcc2(v10003,S(c));gAddAcc1(Z,b,c)=gAddAcc2(b,c);gAddAcc1(S(v10000),b,c)=gAddAcc1(v10000,S(b),c);)"

testAdvFromGeneral : IO Bool
testAdvFromGeneral = testAS
  "f(x) = f(S(x));"
  "f(a)"
  "(f1(a), f1(a)=f1(S(a));)"

testAdvFromEmb : IO Bool
testAdvFromEmb = testAS
  "f(x) = g(f(x));g(A) = B;"
  "f(a)"
  "(f1(a), g2(A)=B;f1(a)=g2(f1(a));)"

export
allTests : IO ()
allTests = runTests
  [ testAddAB
  , testAddAdd
  , testAddAccAB
  , testAdvAddAB
  , testAdvAddAA
  , testAdvAddAdd
  , testAdvAddAccAB
  , testAdvAddAccAA
  , testAdvAddAccAddAcc
  , testAdvFromGeneral
  -- , testAdvFromEmb
  ]
