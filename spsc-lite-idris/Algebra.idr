module Algebra

import Data.SortedMap
import Control.Monad.State

import SLanguage

%default total

%access public export

-- Substitutions

Subst : Type
Subst = SortedMap Name Exp

implementation Eq Subst where
  (==) subst1 subst2 = toList subst1 == toList subst2

theSameFunctor : Exp -> Exp -> Bool
theSameFunctor (Call kind1 name1 _) (Call kind2 name2 _) =
  kind1 == kind2 && name1 == name2
theSameFunctor _ _ = False

applySubst : Subst -> Exp -> Exp
applySubst m e = case e of
  Var name =>
    fromMaybe e (lookup name m)
  Call kind name args =>
    Call kind name (map (assert_total $ applySubst m) args)
  Let e bindings =>
    idris_crash "Algebra.applySubst"

-- Matching

mutual

  matchAgainstAcc : Maybe Subst -> Exp -> Exp -> Maybe Subst
  matchAgainstAcc (Just m) (Var vname) e' =
    case lookup vname m of
      Nothing => Just $ insert vname e' m
      Just e'' =>
        if e' /= e'' then Nothing else Just m
  matchAgainstAcc (Just m) (Call kind name args) (Call kind' name' args') =
    if kind == kind' && name == name'
          then matchAgainstAccL (Just m) args args' else Nothing
  matchAgainstAcc _ _ _ = Nothing

  matchAgainstAccL : Maybe Subst -> Args -> Args -> Maybe Subst
  matchAgainstAccL (Just m) [] [] = (Just m)
  matchAgainstAccL (Just m) (e :: es) (e' :: es') =
    matchAgainstAccL (matchAgainstAcc (Just m) e e') es es'
  matchAgainstAccL _ _ _ = Nothing

matchAgainst : Exp -> Exp -> Maybe Subst
matchAgainst e e' = matchAgainstAcc (Just empty) e e'

instOf : Exp -> Exp -> Bool
instOf e' e =
  case matchAgainst e e' of
    Nothing => False
    Just _ => True

equiv : Exp -> Exp -> Bool
equiv e1 e2 = (e1 `instOf` e2) && (e2 `instOf` e1)

-- Free variables

vars : Exp -> List Name
vars (Var vname) = [vname]
vars (Call _ _ args) =
  foldl union [] (assert_total $ map vars args)
vars (Let e bs) = []

-- Function calls

isFGCall : Exp -> Bool
isFGCall (Call FCall _ _) = True
isFGCall (Call GCall _ _) = True
isFGCall _ = False 

-- Fresh names

mkName : Nat -> String
mkName k = "v" ++ show k

freshName : State Nat Name
freshName =
  do k <- get 
     put $ S k
     pure $ mkName k

freshNameList : Nat -> State Nat (List Name)
freshNameList n =
  do k <- get
     put $ n + k
     pure $ if n == Z then [] else map mkName [k .. pred (k + n)]
