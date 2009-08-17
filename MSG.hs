-- Most Specific Generalization
module MSG where

import qualified Data.Map as Map
import Control.Monad.State

import SParsers
import SLanguage
import ShowUtil
import Algebra

data Gen = Gen Exp Subst Subst
             deriving Eq

msg :: Exp -> Exp -> State Int Gen

msg e1 e2 =
  do k <- freshName
     let u = Gen (Var k) (Map.singleton k e1) (Map.singleton k e2) in
       msgLoop u

msgLoop :: Gen -> State Int Gen

msgLoop u =
  do let u' = mergeSubexp u
     u'' <- commonFunctor u'
     if u'' == u then return u
                 else msgLoop u''

commonFunctor :: Gen -> State Int Gen

commonFunctor u @ (Gen e m1 m2) =
  case [(k, e1) | (k, e1) <- Map.assocs m1, theSameFunctor e1 (m2 Map.! k)] of
    [] -> do { return u}
    (k, e1@(Call ctr name args1)) : _ ->
      let e2@(Call _ _ args2) = m2 Map.! k in
      do ns <- freshNameList (length args1)
         let vs = map Var ns
             e' = applySubst (Map.singleton k (Call ctr name vs)) e
             m1' = (Map.delete k m1) `Map.union` Map.fromList (ns `zip` args1)
             m2' = (Map.delete k m2) `Map.union` Map.fromList (ns `zip` args2) in
           return $ Gen e' m1' m2'


mergeableKeyPairs m1 m2 =
  [ (k1,k2) | (k1,e1) <- Map.assocs m1, (k2,e2) <- Map.assocs m1, k1 < k2,
               e1 == e2, m2 Map.! k1 == m2 Map.! k2]

mergeSubexp u @ (Gen e m1 m2) =
  case mergeableKeyPairs m1 m2 of
    [] -> u
    (k1, k2) : _ ->
      let e' = applySubst (Map.singleton k1 (Var k2)) e
          m1' = Map.delete k1 m1
          m2' = Map.delete k1 m2
      in Gen e' m1' m2'

---- Show

instance Show Gen where
    show (Gen e subst1 subst2) =
      show e ++ " =>> "
        ++ "{" ++ showBindings (Map.toAscList subst1) ++ "}"
        ++ "{" ++ showBindings (Map.toAscList subst2) ++ "}" 
  