module SLanguageTests

import Test.Unit

import SLanguage

shEq : Show a => a -> String -> IO Bool
shEq g e = assertEquals (show g) e

Test101VarAndCall : IO Bool
Test101VarAndCall =
  do shEq (Var "x") "x"
     shEq (Call Ctr "A" [Var "x", Var "y"]) "A(x,y)"
     shEq (Call Ctr "C" []) "C"
     shEq (Call FCall "fX" [Var "x", Var "y"]) "fX(x,y)"
     shEq (Call GCall "gX" [Var "x", Var "y"]) "gX(x,y)"

Test102Let : IO Bool
Test102Let =
  do shEq (Let (Var "y") [("x", Var "y")]) "let x=y in y"
     shEq (Let (Var "x") [("x", Var "a"), ("y", Var "b")])
          "let x=a,y=b in x"

Test103Rule : IO Bool
Test103Rule =
  do shEq (FRule "f" ["x", "y"] (Var "y")) "f(x,y)=y;"
     shEq (GRule "g" "C" ["x"] ["y"] (Var "y")) "g(C(x),y)=y;"
     shEq (GRule "g" "C" [] ["y"] (Var "y")) "g(C,y)=y;"
     shEq (GRule "g" "C" [] [] (Call Ctr "C" []))   "g(C)=C;"

Test104Program : IO Bool
Test104Program =
  do shEq (MkProgram
            [ FRule "f" [] (Call Ctr "A" [])
            , FRule "f1" [] (Call Ctr "A1" [])])
          "f()=A;f1()=A1;"
     shEq (MkProgram
            [ GRule"g" "C" [] [] (Call Ctr "A" [])
            , GRule "g1" "C" [] ["x"] (Call Ctr "A" [])
            , GRule "g2" "C" ["x"] [] (Call Ctr "A" [])])
          "g(C)=A;g1(C,x)=A;g2(C(x))=A;"

Test201Eq : IO Bool
Test201Eq =
  do assertTrue (Var "x" == Var "x")
     assertTrue (Var "x" /= Var "y")
     assertTrue (Call Ctr "A" [] == Call Ctr "A" [])
     assertTrue (Call Ctr "A" [] /= Call Ctr "B" [])
     assertTrue (the (List Exp) [] == [])
     assertTrue ([Var "x"] ==  [Var "x"])
     assertTrue ([Var "x"] /= [Var "y"])
     assertTrue ([Var "x"] /= [Var "x", Var "z"])
     assertTrue (Call Ctr "A" [Var "x"] == Call Ctr "A" [Var "x"])
     assertTrue (Call Ctr "A" [Var "x"] /= Call Ctr "A" [Var "y"])

export
allTests : IO ()
allTests = runTests
  [Test101VarAndCall
  , Test102Let
  , Test103Rule
  , Test104Program
  , Test201Eq]
