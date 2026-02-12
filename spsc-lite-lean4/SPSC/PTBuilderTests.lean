import SPSC.Basic
import SPSC.SLanguage
import SPSC.SParser
import SPSC.ProcessTree
import SPSC.PTBuilder

import Batteries
import Std.Data.TreeMap

open Std

open CKind Exp

namespace PTBuilderTests

-- Driving

def runDrStep (p : Program) (e : Exp) : List Branch :=
  (drivingStep p e 100).fst

def evDrStep (givenProg givenExp : String) : Option String := do
  let p ← parseProg givenProg
  let e ← parseExp givenExp
  pure $ toString $ runDrStep p e

def testDrStep (p  e expected : String) : Bool :=
  evDrStep p e == some expected

--

def evBuilder (tb : TreeBuilder) (givenProg givenExp : String) : Option String := do
  let prog ← parseProg givenProg
  let e ← parseExp givenExp
  pure $ tsTree $ tb prog e

--

def buildLoopN : Nat -> BuildLoop -- := fun buildStep prog tree =>
  | 0, _, _, tree => pure tree
  | k + 1, buildStep, prog, tree =>
      match findAnUnprocessedNode tree with
        | none => pure tree
        | some beta => do
            let tree' ← buildStep prog tree beta
            buildLoopN k buildStep prog tree'

def basicBuilderN (k : Nat) : TreeBuilder :=
  mkTreeBuilder (buildLoopN k) basicBuildStep

def advancedBuilderN (k :Nat) : TreeBuilder :=
  mkTreeBuilder (buildLoopN k) advancedBuildStep

def evBB : String -> String -> Option String :=
  evBuilder basicBuilder

def testBB (prog e expected : String) : Bool :=
  evBB prog e == some expected

def evBBN (k : Nat) (prog e : String) : Option String :=
  evBuilder (basicBuilderN k) prog e

def testBBN (k : Nat) (prog e expected : String) : Bool :=
  evBBN k prog e == some expected

def evBB1 : String -> String -> Option String :=
  evBBN 1

def testBB1 : String -> String -> String -> Bool :=
  testBBN 1

def evAB (prog e : String) : Option String :=
  evBuilder advancedBuilder prog e

def testAB (prog e expected : String) : Bool :=
  evAB prog e == some expected

def evABN (k : Nat) (prog e : String) : Option String :=
  evBuilder (advancedBuilderN k) prog e

def testABN (k : Nat) (prog e expected : String) : Bool :=
  evABN k prog e == some expected

def testAB1 : String -> String -> String -> Bool :=
  testABN 1

-- Example programs

def pAdd := "gAdd(Z,y)=y;gAdd(S(x),y)=S(gAdd(x,y));"

def pAddAcc := "gAddAcc(Z,y)=y;gAddAcc(S(x),y)=gAddAcc(x,S(y));"

def pXX := "g1(C(x))=B;g2(B,x)=x;"

-- Driving

-- Ctr

#guard testDrStep
  ""
  "C(a,b)"
  "[(a, none), (b, none)]"

-- FCall

#guard testDrStep
  "f(x)=x;"
  "f(A(z))"
  "[(A(z), none)]"

#guard testDrStep
  "f(x)=f(S(x));"
  "f(a)"
  "[(f(S(a)), none)]"

-- GCall Ctr

#guard testDrStep
  pAddAcc
  "gAddAcc(S(S(Z)), Z)"
  "[(gAddAcc(S(Z),S(Z)), none)]"

-- GCall Var

#guard testDrStep
  pAddAcc
  "gAddAcc(a,b)"
  "[(b, some (a = Z)), (gAddAcc(v100,S(b)), some (a = S(v100)))]"

-- GCall general

#guard testDrStep
  pAddAcc
  "gAddAcc(gAddAcc(a, b), c)"
  "[(gAddAcc(b,c), some (a = Z)), (gAddAcc(gAddAcc(v100,S(b)),c), some (a = S(v100)))]"

-- Basic builder

#guard testBB "" "x"
  "[{0^_: x}]"

#guard testBB "" "S(Z)"
  "[{0^_: S(Z) @[10000]}, {10000^0: Z}]"

#guard testBB "f(x)=f(S(x));" "f(a)"
  "[{0^_: f(a) @[10000]}, {10000^0: let a=S(a) in f(a) @[10001, 10002]}, {10001^10000^^0: f(a)}, {10002^10000: S(a) @[10003]}, {10003^10002: a}]"

#guard testBB pAddAcc "gAddAcc(S(Z), Z)"
  "[{0^_: gAddAcc(S(Z),Z) @[10000]}, {10000^0: gAddAcc(Z,S(Z)) @[10001]}, {10001^10000: S(Z) @[10002]}, {10002^10001: Z}]"

-- Advanced builder

#guard testAB1 "" "x"
  "[{0^_: x}]"

#guard testAB1 "f(x)=f(S(x));" "f(a)"
  "[{0^_: f(a) @[10000]}, {10000^0: f(S(a))}]"

#guard testAB1 "" "S(Z)"
  "[{0^_: S(Z) @[10000]}, {10000^0: Z}]"

#guard testAB "f(x)=f(S(x));" "f(a)"
  "[{0^_: f(a) @[10000]}, {10000^0: let a=S(a) in f(a) @[10001, 10002]}, {10001^10000^^0: f(a)}, {10002^10000: S(a) @[10003]}, {10003^10002: a}]"

#guard testAB "f(x) = g(f(x));g(A) = B;" "f(a)"
  "[{0^_: f(a) @[10000]}, {10000^0: let v10002=f(a) in g(v10002) @[10003, 10004]}, {10003^10000: g(v10002) @[10005]}, {10004^10000^^0: f(a)}, {10005^10003: B ?v10002 = A}]"

#guard testAB1 pAddAcc "gAddAcc(S(Z), Z)"
  "[{0^_: gAddAcc(S(Z),Z) @[10000]}, {10000^0: gAddAcc(Z,S(Z))}]"

#guard testAB1 pAdd "gAdd(a, b)"
  "[{0^_: gAdd(a,b) @[10001, 10002]}, {10001^0: b ?a = Z}, {10002^0: S(gAdd(v10000,b)) ?a = S(v10000)}]"

#guard testAB1 pAdd "gAdd(gAdd(a,b),c)"
  "[{0^_: gAdd(gAdd(a,b),c) @[10001, 10002]}, {10001^0: gAdd(b,c) ?a = Z}, {10002^0: gAdd(S(gAdd(v10000,b)),c) ?a = S(v10000)}]"

#guard testAB pXX "g2(g1(a),a)"
  "[{0^_: g2(g1(a),a) @[10001]}, {10001^0: g2(B,C(v10000)) ?a = C(v10000) @[10002]}, {10002^10001: C(v10000) @[10003]}, {10003^10002: v10000}]"

end PTBuilderTests
