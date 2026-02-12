import SPSC.SParser
import SPSC.SLanguage
import SPSC.MSG

def evMSG (g1 g2 : String) : Option String := do
  let e1 ← parseExp g1
  let e2 ← parseExp g2
  pure $ toString $ (msg e1 e2 10000).fst

def testMSG (g1 g2 expected : String) : Bool :=
  evMSG g1 g2 == some expected

-- Common functor

#guard testMSG
  "A(a1,C(a2,a3))"
  "A(b1,C(b2,b3))"
  "A(v10001,C(v10003,v10004)) =>> {v10001=a1,v10003=a2,v10004=a3}{v10001=b1,v10003=b2,v10004=b3}"

-- Merge subexp

#guard testMSG
  "f(a1,a2,a1)"
  "f(b1,b2,b1)"
  "f(v10003,v10002,v10003) =>> {v10002=a2,v10003=a1}{v10002=b2,v10003=b1}"

#guard testMSG
  "f(a,a)"
  "f(b,S(b))"
  "f(v10001,v10002) =>> {v10001=a,v10002=a}{v10001=b,v10002=S(b)}"
