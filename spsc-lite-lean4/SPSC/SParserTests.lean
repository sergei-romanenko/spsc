import SPSC.SLanguage
import SPSC.SParser

open CKind Exp Program SLLTask

private
def pExp (input : String) (expected : Exp) : Bool :=
  parseExp input == some expected

private
def pProg (input : String) (expected : Program) : Bool :=
  parseProg input == some expected

private
def pTask (input : String) (expected : SLLTask) : Bool :=
  parseTask input == some expected

private
def pWS (input : String) : Bool :=
  (parseTask input).isSome

#guard pExp "x" (Var "x")
#guard pExp "A" (Call (Ctr) "A" [])
#guard pExp "A()" (Call (Ctr) "A" [])
#guard pExp "A(x)" (Call (Ctr) "A" [Var "x"])
#guard pExp "A(x,y)" (Call (Ctr) "A" [Var "x", Var "y"])
#guard pExp "fX(x)" (Call (FCall) "fX" [Var "x"])
#guard pExp "fX(x,y,z)" (Call (FCall) "fX" [Var "x", Var "y", Var "z"])
#guard pExp "gX(x)" (Call (GCall) "gX" [Var "x"])
#guard pExp "gX(x,y,z)" (Call (GCall) "gX" [Var "x", Var "y", Var "z"])

#guard pProg "g(C)=D;f(x)=x;" $ Program.mk
  [ FRule.mk "f" ["x"] (Var "x") ]
  [ GRule.mk "g" "C" [] [] (Call (CKind.Ctr) "D" []) ]

#guard pProg "f(x,y)=A(x,y);" $ Program.mk
  [ FRule.mk "f" ["x", "y"] (Call (Ctr) "A" [Var "x", Var "y"]) ] []
#guard pProg "f(x)=A();" $ Program.mk
  [ FRule.mk "f" ["x"] (Call (Ctr) "A" []) ] []
#guard pProg "f(x)=A;" $ Program.mk
  [ FRule.mk "f" ["x"] (Call (Ctr) "A" []) ] []
#guard pProg "g(C(x,y),z)=x;" $ Program.mk
  [] [ GRule.mk "g" "C" ["x", "y"] ["z"] (Var "x") ]
#guard pProg "g(C(x,y))=x;" $ Program.mk
  [] [ GRule.mk "g" "C" ["x", "y"] [] (Var "x") ]
#guard pProg "g(C)=x;" $ Program.mk
  [] [ GRule.mk "g" "C" [] [] (Var "x") ]

#guard pTask "g(x) where g(C)=D;f(x)=x;" (SLLTask.mk
  (Call (GCall) "g" [Var "x"])
  (Program.mk
    [ FRule.mk "f" ["x"] (Var "x") ]
    [ GRule.mk "g" "C" [] [] (Call Ctr "D" []) ]))

#guard pExp "A( \n\r\t )" (Call (Ctr) "A" [])

#guard pWS $
  "g( C( x , y ) , z )\nwhere " ++
  "g( C( x , y ) , x ) = D( A , B ) ; f( x , y ) = x ; "
