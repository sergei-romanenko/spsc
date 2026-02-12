import SPSC.SLanguage

--- toString

private
def tsEq {α} [ToString α] (g : α) (e : String) : Bool :=
  toString g == e

open CKind Exp Program SLLTask

#guard tsEq (Var "x") "x"
#guard tsEq (Call Ctr "A" [Var "x", Var "y"]) "A(x,y)"
#guard tsEq (Call Ctr "C" []) "C"
#guard tsEq (Call FCall "fX" [Var "x", Var "y"]) "fX(x,y)"
#guard tsEq (Call GCall "gX" [Var "x", Var "y"]) "gX(x,y)"

#guard tsEq (Let (Var "y") [("x", Var "y")])
        "let x=y in y"
#guard tsEq (Let (Var "x") [("x", Var "a"), ("y", Var "b")])
        "let x=a,y=b in x"

#guard tsEq (FRule.mk "f" ["x", "y"] (Var "y")) "f(x,y)=y;"
#guard tsEq (GRule.mk "g" "C" ["x"] ["y"] (Var "y")) "g(C(x),y)=y;"
#guard tsEq (GRule.mk "g" "C" [] ["y"] (Var "y")) "g(C,y)=y;"
#guard tsEq (GRule.mk "g" "C" [] [] (Call Ctr "C" []))   "g(C)=C;"

#guard tsEq (Program.mk
              [ FRule.mk "f" [] (Call Ctr "A" [])
              , FRule.mk "f1" [] (Call Ctr "A1" [])]
              [])
            "f()=A;f1()=A1;"
#guard tsEq (Program.mk
              []
              [ GRule.mk "g" "C" [] [] (Call Ctr "A" [])
              , GRule.mk "g1" "C" [] ["x"] (Call Ctr "A" [])
              , GRule.mk "g2" "C" ["x"] [] (Call Ctr "A" [])])
            "g(C)=A;g1(C,x)=A;g2(C(x))=A;"

#guard tsEq (SLLTask.mk
   (Var "x")
   (Program.mk [FRule.mk "f" [] (Call Ctr "A" [])] []))
  "x where f()=A;"

--- BEq ---

#guard Var "x" == Var "x"
#guard Var "x" != Var "y"
#guard Call Ctr "A" [] == Call Ctr "A" []
#guard Call Ctr "A" [] != Call Ctr "B" []
#guard ([] : List Exp) == []
#guard [Var "x"] ==  [Var "x"]
#guard [Var "x"] != [Var "y"]
#guard [Var "x"] != [Var "x", Var "z"]
#guard Call Ctr "A" [Var "x"] == Call Ctr "A" [Var "x"]
#guard Call Ctr "A" [Var "x"] != Call Ctr "A" [Var "y"]
