module MSG

-- Most Specific Generalization

import Data.SortedMap
import Control.Monad.State

import SParsers
import SLanguage
import Algebra

public export
data Gen = MkGen Exp Subst Subst

implementation Eq Gen where
  (==) (MkGen e1 a1 b1) (MkGen e2 a2 b2) =
    e1 == e2 && a1 == a2 && b1 == b2

export
implementation Show Gen where
    show (MkGen e subst1 subst2) =
      show e ++ " =>> "
        ++ "{" ++ showBindings (toList subst1) ++ "}"
        ++ "{" ++ showBindings (toList subst2) ++ "}" 

mergeableKeyPairs : (Ord t, Eq a) => (m1, m2 : SortedMap t a) -> List (t, t)
mergeableKeyPairs m1 m2 =
  [ (k1, k2) | (k1, e1) <- toList m1, (k2, e2) <- toList m1, k1 < k2,
               e1 == e2, lookup k1 m2 == lookup k2 m2]

mergeSubexp : Gen -> Gen
mergeSubexp u @ (MkGen e m1 m2) =
  case mergeableKeyPairs m1 m2 of
    [] => u
    (k1, k2) :: _ =>
      let e' = applySubst (insert k1 (Var k2) empty) e
          m1' = delete k1 m1
          m2' = delete k1 m2
      in MkGen e' m1' m2'

mergeableFunctors : Gen -> List (Name, Exp)
mergeableFunctors u@(MkGen e m1 m2) =
  [(k, e1) | (k, e1) <- toList m1,
             let Just e2 = lookup k m2, theSameFunctor e1 e2]

commonFunctor : Gen -> State Nat Gen
commonFunctor u @ (MkGen e m1 m2) =
  case mergeableFunctors u of
    [] => pure u
    (k, e1) :: _ =>
      let Call ctr name args1 = e1 in
      let Just (e2@(Call _ _ args2)) = lookup k m2 in
      do ns <- freshNameList (length args1)
         let vs = map Var ns
         let e' = applySubst (insert k (Call ctr name vs) empty) e
         let m1' = insertFrom (ns `zip` args1) (delete k m1)
         let m2' = insertFrom (ns `zip` args2) (delete k m2)
         pure $ MkGen e' m1' m2'

msgLoop : Gen -> State Nat Gen
msgLoop u =
  do u2 <- commonFunctor (mergeSubexp u)
     if u2 == u then pure u
                else msgLoop u2

export
msg : Exp -> Exp -> State Nat Gen
msg e1 e2 =
  do k <- freshName
     let u = MkGen (Var k) (insert k e1 empty) (insert k e2 empty)
     msgLoop u
