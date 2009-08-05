module Algebra where

import qualified Data.Map as M
import qualified Data.List as List
import Control.Monad.State


import SLanguage

theSameFunctor :: Exp -> Exp -> Bool

theSameFunctor (Call kind1 name1 _) (Call kind2 name2 _) =
  kind1 == kind2 && name1 == name2
theSameFunctor _ _ = False

substExp :: M.Map Name Exp -> Exp -> Exp

substExp m e =
  case e of
    Var name ->
      M.findWithDefault e name m
    Call kind name args ->
      Call kind name (map (substExp m) args)

equiv :: Exp -> Exp -> Bool

equiv e1 e2 = e1 `instOf` e2 && e2 `instOf` e1 

instOf :: Exp -> Exp -> Bool

instOf e' e =
  case matchAgainst e e' of
    Nothing -> False
    Just _ -> True

matchAgainst :: Exp -> Exp -> Maybe (M.Map Name Exp)

matchAgainst e e' = matchAgainstAcc (Just M.empty) e e'

matchAgainstAcc :: Maybe (M.Map Name Exp) -> Exp -> Exp ->
                   Maybe (M.Map Name Exp)

matchAgainstAcc (Just m) (Var vname) e' =
  case M.lookup vname m of
    Nothing -> Just $ M.insert vname e' m
    Just e'' ->
      if e' /= e'' then Nothing else Just m
matchAgainstAcc (Just m) (Call kind name args) (Call kind' name' args')
  | kind == kind' && name == name' =
        matchAgainstAccL (Just m) args args'
matchAgainstAcc _ _ _ = Nothing

matchAgainstAccL :: Maybe (M.Map Name Exp) -> Args -> Args ->
                    Maybe (M.Map Name Exp)

matchAgainstAccL (Just m) [] [] = (Just m)
matchAgainstAccL (Just m) (e : es) (e' : es') =
  matchAgainstAccL (matchAgainstAcc (Just m) e e') es es'
matchAgainstAccL _ _ _ = Nothing

vars :: Exp -> [Name]
vars (Var vname) = [vname]
vars (Call _ _ args) =
  foldl List.union [] (map vars args)

isFGCall :: Exp -> Bool

isFGCall (Call FCall _ _) = True
isFGCall (Call GCall _ _) = True
isFGCall _ = False 

mkName :: (Show a) => a -> [Char]

mkName t = "$" ++ show t

freshName :: State Int Name
freshName =
  do t <- get
     put $ t+1
     return $ mkName t

freshNameList :: (MonadState a m, Num a, Enum a) => a -> m [String]

freshNameList n =
  do t <- get
     put $ t + n
     return $ map mkName [t..(t+n-1)]
