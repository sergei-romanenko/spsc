module MSG_Tests where

import Test.HUnit
import Control.Monad.State

import SParsers
import SLanguage
import MSG

msgTests = TestLabel "MSG" ( TestList [
  testMSG_commonFunctor, testMSG_mergeSubexp1, testMSG_mergeSubexp2
  ] )

evalMSG e1 e2 = (evalState $ msg e1 e2) 10000

testMSG label e1 e2 expected = TestCase $ assertEqual
  label expected (show (evalMSG (pExp e1) (pExp e2)))

testMSG_commonFunctor = testMSG "commonFunctor"
  "A(a1,C(a2,a3))"
  "A(b1,C(b2,b3))"
  "A(v10001,C(v10003,v10004)) =>> {v10001=a1,v10003=a2,v10004=a3}{v10001=b1,v10003=b2,v10004=b3}"

testMSG_mergeSubexp1 = testMSG "mergeSubexp1"
  "f(a1,a2,a1)"
  "f(b1,b2,b1)"
  "f(v10003,v10002,v10003) =>> {v10002=a2,v10003=a1}{v10002=b2,v10003=b1}"

testMSG_mergeSubexp2 = testMSG "mergeSubexp2"
  "f(a,a)"
  "f(b,S(b))"
  "f(v10001,v10002) =>> {v10001=a,v10002=a}{v10001=b,v10002=S(b)}"
