import SPSC.SLanguage
import SPSC.SParser
import SPSC.ProcessTree
import SPSC.PTBuilder
import SPSC.ResProgGen

namespace ResProgGenTests

-- Runners

def ScpRunner := Program -> Exp -> (Program × Exp)

def evScp (treeBuilder : TreeBuilder) (givenProg givenExp : String) : Option String := do
  let prog ← parseProg givenProg
  let e ← parseExp givenExp
  pure $ toString $ (genResidualProgram $ treeBuilder prog e)

def evBS : String -> String -> Option String :=
  evScp basicBuilder

def testBS (prog e expected : String) : Bool :=
  evBS prog e == (some expected)

def evAS : String -> String -> Option String :=
  evScp advancedBuilder

def testAS (prog e expected : String) : Bool :=
  evAS prog e == some expected

---- Sample programs

def pAdd := "gAdd(Z,y)=y;gAdd(S(x),y)=S(gAdd(x,y));"

def pAddAcc := "gAddAcc(Z,y)=y;gAddAcc(S(x),y)=gAddAcc(x,S(y));"

---- Basic supercompiler

#guard testBS
  pAdd
  "gAdd(a, b)"
  "(gAdd1(a,b), gAdd1(Z,b)=b;gAdd1(S(v10000),b)=S(gAdd1(v10000,b));)"

#guard testBS
  pAdd
  "gAdd(gAdd(a,b),c)"
  "(gAdd1(a,b,c), gAdd2(Z,c)=c;gAdd2(S(v10003),c)=S(gAdd2(v10003,c));gAdd1(Z,b,c)=gAdd2(b,c);gAdd1(S(v10000),b,c)=S(gAdd1(v10000,b,c));)"

#guard testBS
  pAddAcc
  "gAddAcc(a, b)"
  "(gAddAcc1(a,b), gAddAcc1(Z,b)=b;gAddAcc1(S(v10000),b)=gAddAcc1(v10000,S(b));)"

---- Advanced supercompiler

#guard testAS
  pAdd
  "gAdd(a, b)"
  "(gAdd1(a,b), gAdd1(Z,b)=b;gAdd1(S(v10000),b)=S(gAdd1(v10000,b));)"

#guard testAS
  pAdd
  "gAdd(a, a)"
  "(gAdd1(a,a), gAdd1(Z,v10006)=v10006;gAdd1(S(v10010),v10006)=S(gAdd1(v10010,v10006));)"

#guard testAS
  pAdd
  "gAdd(gAdd(a,b),c)"
  "(gAdd1(a,b,c), gAdd2(Z,c)=c;gAdd2(S(v10003),c)=S(gAdd2(v10003,c));gAdd1(Z,b,c)=gAdd2(b,c);gAdd1(S(v10000),b,c)=S(gAdd1(v10000,b,c));)"

#guard testAS
  pAddAcc
  "gAddAcc(a, b)"
  "(gAddAcc1(a,b), gAddAcc1(Z,b)=b;gAddAcc1(S(v10000),b)=gAddAcc1(v10000,S(b));)"

#guard testAS
  pAddAcc
  "gAddAcc(a, a)"
  "(gAddAcc1(a,a), gAddAcc1(Z,v10005)=v10005;gAddAcc1(S(v10009),v10005)=gAddAcc1(v10009,S(v10005));)"

#guard testAS
  pAddAcc
  "gAddAcc(gAddAcc(a,b),c)"
  "(gAddAcc1(a,b,c), gAddAcc2(Z,c)=c;gAddAcc2(S(v10003),c)=gAddAcc2(v10003,S(c));gAddAcc1(Z,b,c)=gAddAcc2(b,c);gAddAcc1(S(v10000),b,c)=gAddAcc1(v10000,S(b),c);)"

#guard testAS
  "f(x) = f(S(x));"
  "f(a)"
  "(f1(a), f1(a)=f1(S(a));)"

#guard testAS
  "f(x) = g(f(x));g(A) = B;"
  "f(a)"
  "(f1(a), f1(a)=g2(f1(a));g2(A)=B;)"

end ResProgGenTests
