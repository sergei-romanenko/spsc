module MSGTests

import Control.Monad.State
import Test.Unit

import SParsers
import SLanguage
import MSG

evMSG : String -> String -> Maybe String
evMSG g1 g2 =
  do e1 <- parseExp g1
     e2 <- parseExp g2
     pure $ show $ (evalState $ msg e1 e2) 10000

testMSG : String -> String -> String -> IO Bool
testMSG g1 g2 expected =
  assertEquals (evMSG g1 g2) (Just expected)

testMSG_commonFunctor : IO Bool
testMSG_commonFunctor = testMSG
  "A(a1,C(a2,a3))"
  "A(b1,C(b2,b3))"
  "A(v10001,C(v10003,v10004)) =>> {v10001=a1,v10003=a2,v10004=a3}{v10001=b1,v10003=b2,v10004=b3}"

testMSG_mergeSubexp1 : IO Bool
testMSG_mergeSubexp1 = testMSG
  "f(a1,a2,a1)"
  "f(b1,b2,b1)"
  "f(v10003,v10002,v10003) =>> {v10002=a2,v10003=a1}{v10002=b2,v10003=b1}"

testMSG_mergeSubexp2 : IO Bool
testMSG_mergeSubexp2 = testMSG
  "f(a,a)"
  "f(b,S(b))"
  "f(v10001,v10002) =>> {v10001=a,v10002=a}{v10001=b,v10002=S(b)}"

export
allTests : IO ()
allTests = runTests
  [ testMSG_commonFunctor
  , testMSG_mergeSubexp1
  , testMSG_mergeSubexp2
  ]
