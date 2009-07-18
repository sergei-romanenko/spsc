-- Most General Unifier
module MGU where

import SLanguage
import SLanguageShow
import Algebra

import qualified Data.Map as Map

import Control.Monad.State

data Uni = Uni Exp (Map.Map Name Exp) (Map.Map Name Exp)
             deriving (Eq, Show)

mgu :: Exp -> Exp -> State Int Uni

mgu e1 e2 =
  do k <- freshName
     let u = Uni (Var k) (Map.singleton k e1) (Map.singleton k e2) in
       mguLoop u

mguLoop :: Uni -> State Int Uni

mguLoop u =
  do u' <- return (mergeSubexp u)
     u'' <- commonFunctor u'
     if u'' == u then return u
                 else mguLoop u''

commonFunctor :: Uni -> State Int Uni

commonFunctor u @ (Uni e m1 m2) =
  case [(k, e1) | (k, e1) <- Map.assocs m1, theSameFunctor e1 (m2 Map.! k)] of
    [] -> do { return u}
    (k, e1@(Call ctr name args1)) : _ ->
      let e2@(Call _ _ args2) = m2 Map.! k in
      do ns <- freshNameList (length args1)
         let vs = map (Var) ns
             e' = substExp (Map.singleton k (Call ctr name vs)) e
             m1' = (Map.delete k m1) `Map.union` Map.fromList (ns `zip` args1)
             m2' = (Map.delete k m1) `Map.union` Map.fromList (ns `zip` args2) in
           return $ Uni e' m1' m2'


mergeableKeyPairs m1 m2 =
  [ (k1,k2) | (k1,e1) <- Map.assocs m1, (k2,e2) <- Map.assocs m1, k1 < k2,
               e1 == e2, m2 Map.! k2 == m2 Map.! k2]

mergeSubexp u @ (Uni e m1 m2) =
  case mergeableKeyPairs m1 m2 of
    [] -> u
    (k1, k2) : _ ->
      let e' = substExp (Map.singleton k1 (Var k2)) e
          m1' = Map.delete k1 m1
          m2' = Map.delete k1 m2
      in Uni e' m1' m2'


mgu1 = evalState $ mgu
  (Call Ctr "A" [Var "a1", Call Ctr "C" [Var "a2", Var "a3"]])
  (Call Ctr "A" [Var "b1", Call Ctr "C" [Var "b2", Var "b3"]])

mgu2 = evalState $ mgu
  (Call Ctr "f" [Var "a1",Var "a2",Var "a1"])
  (Call Ctr "f" [Var "b1",Var "b2",Var "b1"])
