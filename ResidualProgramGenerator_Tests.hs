module ResidualProgramGenerator_Tests where

import Test.HUnit

import ResidualProgramGenerator

import SLanguage
import SParsers
import Supercompiler

scpTests = TestList [basicScpTests, advancedScpTests]

---- Sample programs
  
pAdd = "gAdd(Z,y)=y;gAdd(S(x),y)=S(gAdd(x,y));"
pAddAcc = "gAddAcc(Z,y)=y;gAddAcc(S(x),y)=gAddAcc(x,S(y));"

---- Basic supercompiler

basicScpTests = TestLabel "BasicScp" ( TestList [
  testAddAB, testAddAdd, testAddAccAB
  ] )

testBasicScp label prog e expected = TestCase $ assertEqual
  label expected (show (runBasicScp (pProg prog) (pExp e)))

runBasicScp prog e =
  genResidualProgram $ basic_buildProcessTree prog e

testAddAB = testBasicScp "AddAB"
  pAdd
  "gAdd(a, b)"
  "(gAdd1(Z,b)=b;gAdd1(S(v10000),b)=S(gAdd1(v10000,b));,gAdd1(a,b))"

testAddAdd = testBasicScp "AddAdd"
  pAdd
  "gAdd(gAdd(a,b),c)"
  "(gAdd2(Z,c)=c;gAdd2(S(v10003),c)=S(gAdd2(v10003,c));gAdd1(Z,b,c)=gAdd2(b,c);gAdd1(S(v10000),b,c)=S(gAdd1(v10000,b,c));,gAdd1(a,b,c))"

testAddAccAB = testAdvScp "AdvAddAccAB"
  pAddAcc
  "gAddAcc(a, b)"
  "(gAddAcc1(Z,b)=b;gAddAcc1(S(v10000),b)=gAddAcc1(v10000,S(b));,gAddAcc1(a,b))"

---- Advanced supercompiler

advancedScpTests = TestLabel "AdvancedScp" ( TestList [
  testAdvAddAB, testAdvAddAdd, testAdvAddAccAB, testAdvAddAccAddAcc 
  ] )

testAdvScp label prog e expected = TestCase $ assertEqual
  label expected (show (runAdvScp (pProg prog) (pExp e)))

runAdvScp prog e =
  genResidualProgram $ advanced_buildProcessTree prog e

testAdvAddAB = testAdvScp "AdvAddAB"
  pAdd
  "gAdd(a, b)"
  "(gAdd1(Z,b)=b;gAdd1(S(v10000),b)=S(gAdd1(v10000,b));,gAdd1(a,b))"

testAdvAddAA = testAdvScp "AdvAddAB"
  pAdd
  "gAdd(a, a)"
  "(gAdd1(Z,v10006)=v10006;gAdd1(S(v10010),v10006)=S(gAdd1(v10010,v10006));,gAdd1(a,a))"

testAdvAddAdd = testAdvScp "AdvAddAdd"
  pAdd
  "gAdd(gAdd(a,b),c)"
  "(gAdd2(Z,c)=c;gAdd2(S(v10003),c)=S(gAdd2(v10003,c));gAdd1(Z,b,c)=gAdd2(b,c);gAdd1(S(v10000),b,c)=S(gAdd1(v10000,b,c));,gAdd1(a,b,c))"

testAdvAddAccAB = testAdvScp "AdvAddAccAB"
  pAddAcc
  "gAddAcc(a, b)"
  "(gAddAcc1(Z,b)=b;gAddAcc1(S(v10000),b)=gAddAcc1(v10000,S(b));,gAddAcc1(a,b))"

testAdvAddAccAA = testAdvScp "AdvAddAccAB"
  pAddAcc
  "gAddAcc(a, a)"
  "(gAddAcc1(Z,v10005)=v10005;gAddAcc1(S(v10009),v10005)=gAddAcc1(v10009,S(v10005));,gAddAcc1(a,a))"

testAdvAddAccAddAcc = testAdvScp "AdvAddAccAddAcc"
  pAddAcc
  "gAddAcc(gAddAcc(a,b),c)"
  "(gAddAcc2(Z,c)=c;gAddAcc2(S(v10003),c)=gAddAcc2(v10003,S(c));gAddAcc1(Z,b,c)=gAddAcc2(b,c);gAddAcc1(S(v10000),b,c)=gAddAcc1(v10000,S(b),c);,gAddAcc1(a,b,c))"
