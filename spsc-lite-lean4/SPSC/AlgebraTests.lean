import SPSC.SLanguage
import SPSC.Algebra
import SPSC.SParser

import Std.Data.TreeMap
open Std

open CKind Exp

private
def testSubst (s : Subst) (given : String) (expected : String) : Bool :=
  ((parseExp given).getD (Var "") |> applySubst s |> toString) == expected

#guard testSubst (.ofList [("x", Call Ctr "A" [Var "a"])])
  "C(x,y)" "C(A(a),y)"

#guard vars <$> parseExp "gAdd(gAdd(x,Z),S(z))" == some ["x", "z"]

def showMatchRes : Option Subst -> String
  | none => "*"
  | some subst =>
      subst.toList |>
        .map (fun ⟨vname, e⟩ => vname ++ "->" ++ toString e ++ ";") |>
          .join

def evMatch (pStr : String) (eStr : String) : Option String := do
  let p ← parseExp pStr
  let e ← parseExp eStr
  pure $ showMatchRes $ e.matchWith p

def testMatch (p : String) (t : String) (expected : String) : Bool :=
  evMatch p t == some expected

#guard testMatch "x" "S(Z)" "x->S(Z);"

#guard testMatch "Z" "x" "*"
#guard testMatch "C(x,y)" "C(A,B)" "x->A;y->B;"
#guard testMatch "C(x,y)" "D(A,B)" "*"
#guard testMatch "C(x,y)" "f(A,B)" "*"
#guard testMatch "C(x,x)" "C(A,A)" "x->A;"
#guard testMatch "C(x,x)" "C(A,B)" "*"

def evEquiv (alpha beta : String) : Option Bool := do
  let e1 ← parseExp alpha
  let e2 ← parseExp beta
  pure (e1.equiv e2)

def testEquiv (a b : String) (expected : Bool) : Bool :=
  evEquiv a b == some expected

#guard testEquiv "gA(fB(x,y),C)" "gA(fB(a,b),C)" true
#guard testEquiv "gA(fB(x,y),x)" "gA(fB(a,a),b)" false

#guard (freshName 25 |> toString) == "(v25, 26)"
#guard (freshNameList 3 100 |> toString) == "([v100, v101, v102], 103)"
