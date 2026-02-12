import SPSC.SLanguage
import SPSC.SParserMeta

import Lean
open Lean

open CKind Exp

-- Exp

elab "test_mkExp " e:exp : term => exExpToTerm $ mkExp e

#guard test_mkExp x == Exp.Var "x"

#guard test_mkExp x == (Var "x")
#guard test_mkExp A == (Call Ctr "A" [])
#guard test_mkExp A() == (Call Ctr "A" [])
#guard test_mkExp A(x) == (Call Ctr "A" [Var "x"])
#guard test_mkExp A(x,y) == (Call Ctr "A" [Var "x", Var "y"])
#guard test_mkExp fX(x) == (Call FCall "fX" [Var "x"])
#guard test_mkExp fX(x,y,z) == (Call FCall "fX" [Var "x", Var "y", Var "z"])
#guard test_mkExp gX(x) == (Call GCall "gX" [Var "x"])
#guard test_mkExp gX(x,y,z) == (Call GCall "gX" [Var "x", Var "y", Var "z"])

-- Prog

elab "test_mkProg " e:program : term => exProgramToTerm $ mkProgram e

#guard test_mkProg g(C)=D;f(x)=x; == Program.mk
  [ FRule.mk "f" ["x"] (Var "x") ]
  [ GRule.mk "g" "C" [] [] (Call (CKind.Ctr) "D" []) ]

#guard test_mkProg f(x,y)=A(x,y); == Program.mk
  [ FRule.mk "f" ["x", "y"] (Call (Ctr) "A" [Var "x", Var "y"]) ] []
#guard test_mkProg f(x)=A(); == Program.mk
  [ FRule.mk "f" ["x"] (Call (Ctr) "A" []) ] []
#guard test_mkProg f(x)=A; == Program.mk
  [ FRule.mk "f" ["x"] (Call (Ctr) "A" []) ] []
#guard test_mkProg g(C(x,y),z)=x; == Program.mk
  [] [ GRule.mk "g" "C" ["x", "y"] ["z"] (Var "x") ]
#guard test_mkProg g(C(x,y))=x; == Program.mk
  [] [ GRule.mk "g" "C" ["x", "y"] [] (Var "x") ]
#guard test_mkProg g(C)=x; == Program.mk
  [] [ GRule.mk "g" "C" [] [] (Var "x") ]

elab "test_mkTask " t:task : term => exTaskToTerm $ mkTask t

#guard test_mkTask g(x) where g(C)=D;f(x)=x; ==
  (SLLTask.mk
    (Call (GCall) "g" [Var "x"])
    (Program.mk
      [ FRule.mk "f" ["x"] (Var "x") ]
      [ GRule.mk "g" "C" [] [] (Call Ctr "D" []) ]))

-- How to run a parser inside MetaM.

def runMkExp (input : String) : MetaM Exp := do
  let env ← getEnv
  match Lean.Parser.runParserCategory env `exp input with
  | .error msg => pure $ Exp.Var $ s!"Parse error: {msg}"
  | .ok e =>
      match mkExp e with
        | .error msg => throwError msg
        | .ok e' => pure e'

-- #eval runMkExp "x"
