import SPSC.SLanguage
import SPSC.Algebra

open CKind Exp

-- This is the "classic" homeomorphic imbedding relation.

mutual

  partial
  def he (e1 e2 :Exp) : Bool :=
    heByDiving e1 e2 || heByCoupling e1 e2

  partial
  def heByDiving : Exp -> Exp -> Bool
    | e1, Call _ _ args => List.any args (he e1)
    | _,  _ => false

  partial
  def heByCoupling : Exp -> Exp -> Bool
  | Var _, Var _ => true
  | Call kind1 name1 args1, Call kind2 name2 args2 =>
      kind1 == kind2 && name1 == name2 &&
        (args1.zip args2).all (fun ⟨e1, e2⟩ => he e1 e2)
  | _, _ => false

end /- mutual -/

-- We distinguish a specific category of expressions:
-- the ones that generate contractions in the process tree.

def aVarIsUnderAttack : Exp -> Bool
  | Var _ => true
  | Call GCall _ (arg :: _) =>
      aVarIsUnderAttack arg
  | _ => false

-- Enhanced homeomorphic embedding:
-- expressions are compared only if they belong
-- to the same category (as defined by `aVarIsUnderAttack`).

def Exp.embeddedIn (e1 e2 : Exp) : Bool :=
  aVarIsUnderAttack e1 == aVarIsUnderAttack e2 && he e1 e2
