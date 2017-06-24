module HE(he, embeddedIn) where

import SLanguage
import Algebra

-- Enhanced homeomorphic embedding:
-- expressions are compared only if they belong
-- to the same category (as defined by `aVarIsUnderAttack`).

embeddedIn e1 e2 =
  aVarIsUnderAttack e1 == aVarIsUnderAttack e2 && he e1 e2 

-- This is the "classic" homeomorphic imbedding relation.

he e1 e2 = heByDiving e1 e2 || heByCoupling e1 e2

heByDiving e1 (Var _) = False
heByDiving e1 (Call _ _ args) = any (he e1) args

heByCoupling (Var _) (Var _) = True
heByCoupling (Call kind1 name1 args1) (Call kind2 name2 args2)
               | kind1 == kind2 && name1 == name2 =
      and (zipWith he args1 args2)
heByCoupling _ _ = False

-- We distinguish a specific category of expressions:
-- the ones that generate contractions in the process tree.

aVarIsUnderAttack (Call GCall _ args) = aVarIsUnderAttack (head args)
aVarIsUnderAttack (Var _) = True
aVarIsUnderAttack _ = False
