module HE_Tests where

import Test.HUnit

import SLanguage
import HE

heTests = TestLabel "HE" ( TestList [
  testVV, testVF, testFV, testDiving, testCoupling1, testCoupling2
  ] )

testVV = TestCase $ assertBool
  "v1 <| v2"
  (he (Var "v1") (Var "v2"))

testVF = TestCase $ assertBool
  "v1 <| f(v2)"
  (he (Var "v1") (Call Ctr "f" [(Var "v2")]))

testFV = TestCase $ assertBool
  "not f(v2) <| v1"
  (not(he (Call Ctr "f" [(Var "v2")]) (Var "v1")))

testDiving = TestCase $ assertBool
  "f(v1) < g(v0, f(h(v2)))"
  (he (Call Ctr "f" [(Var "v1")])
      (Call Ctr "g" [(Var "v0"), (Call Ctr "f" [Call Ctr "h" [(Var "v2")]])]))

testCoupling1 = TestCase $ assertBool
  "f(v1, g(v2)) <| f(h(w1), g(w2))"
  (he (Call Ctr "f" [Var "v1", Call Ctr "g" [Var "v2"]])
      (Call Ctr "f" [Call Ctr "h" [Var "w1"], Call Ctr "g" [Var "w2"]]))

testCoupling2 = TestCase $ assertBool
  "not f(v1) <| g(w1)"
  (not (he (Call Ctr "f" [Var "v1"]) (Call Ctr "g" [Var "w1"])))