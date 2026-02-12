import SPSC.SLanguage
import SPSC.SParser
import SPSC.HE

-- he

def heTrue (a1 a2 : String) : Bool :=
  he <$> parseExp a1 <*> parseExp a2 == some true

def heFalse (a1 a2 : String) : Bool :=
  he <$> parseExp a1 <*> parseExp a2 == some false

#guard heTrue "v1" "v2"
#guard heTrue "v1" "f(v2)"
#guard heFalse "f(v2)" "v1"
#guard heTrue "f(v1)" "f(g(v2))"
#guard heTrue "C()" "C()"
#guard heTrue "C" "D(C)"
#guard heTrue "f(v1)" "g(f(v2))"
#guard heTrue "f(v1)" "g(v0,f(v2))"
#guard heTrue "f(v1)" "g(v0,f(H(v2)))"
#guard heTrue "f(v1,g(v2))" "f(H(w1),g(w2))"
#guard heFalse "f(v1)" "g(w1)"

-- aVarIsUnderAttack

def vaTrue (input : String) : Bool :=
  aVarIsUnderAttack <$> parseExp input == some true

def vaFalse (input : String) : Bool :=
  aVarIsUnderAttack <$> parseExp input == some false

#guard vaTrue "x"
#guard vaFalse "A"
#guard vaFalse "f(x)"
#guard vaTrue "g(x,y)"
#guard vaTrue "g1(g2(x))"
#guard vaFalse "g(f(x))"
