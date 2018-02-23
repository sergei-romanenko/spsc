module HE

import SLanguage
import Algebra

-- This is the "classic" homeomorphic imbedding relation.

mutual

  export
  he : Exp -> Exp -> Bool
  he e1 e2 = heByDiving e1 e2 || heByCoupling e1 e2

  heByDiving : Exp -> Exp -> Bool
  heByDiving e1 (Var _) = False
  heByDiving e1 (Call _ _ args) = any (he e1) args

  heByCoupling : Exp -> Exp -> Bool
  heByCoupling (Var _) (Var _) = True
  heByCoupling (Call kind1 name1 args1) (Call kind2 name2 args2) =
    kind1 == kind2 && name1 == name2 &&
      and (List.zipWith (\e1,e2 => Delay (he e1 e2)) args1 args2)
  heByCoupling _ _ = False

-- We distinguish a specific category of expressions:
-- the ones that generate contractions in the process tree.

export
aVarIsUnderAttack : Exp -> Bool
aVarIsUnderAttack (Call GCall _ (arg :: args)) =
  aVarIsUnderAttack arg
aVarIsUnderAttack (Var _) = True
aVarIsUnderAttack _ = False

-- Enhanced homeomorphic embedding:
-- expressions are compared only if they belong
-- to the same category (as defined by `aVarIsUnderAttack`).

export
embeddedIn : Exp -> Exp -> Bool
embeddedIn e1 e2 =
  aVarIsUnderAttack e1 == aVarIsUnderAttack e2 && he e1 e2 
