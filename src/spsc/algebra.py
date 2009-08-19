'''
Created on Aug 17, 2009

@author: Sergei Romanenko
'''

import itertools

from sll_language import *

#import qualified Data.Map as Map
#import qualified Data.List as List
#import Control.Monad.State
#
#import SLanguage
#
#type Subst = Map.Map Name Exp



#equiv :: Exp -> Exp -> Bool
#
#equiv e1 e2 = e1 `instOf` e2 && e2 `instOf` e1 
#
#instOf :: Exp -> Exp -> Bool
#
#instOf e' e =
#  case matchAgainst e e' of
#    Nothing -> False
#    Just _ -> True


#matchAgainst :: Exp -> Exp -> Maybe (Subst)
#
#matchAgainst e e' = matchAgainstAcc (Just Map.empty) e e'
#
#matchAgainstAcc :: Maybe (Subst) -> Exp -> Exp -> Maybe (Subst)
#
#matchAgainstAcc (Just m) (Var vname) e' =
#  case Map.lookup vname m of
#    Nothing -> Just $ Map.insert vname e' m
#    Just e'' ->
#      if e' /= e'' then Nothing else Just m
#matchAgainstAcc (Just m) (Call kind name args) (Call kind' name' args')
#  | kind == kind' && name == name' =
#        matchAgainstAccL (Just m) args args'
#matchAgainstAcc _ _ _ = Nothing
#
#matchAgainstAccL :: Maybe (Subst) -> Args -> Args -> Maybe (Subst)
#
#matchAgainstAccL (Just m) [] [] = (Just m)
#matchAgainstAccL (Just m) (e : es) (e' : es') =
#  matchAgainstAccL (matchAgainstAcc (Just m) e e') es es'
#matchAgainstAccL _ _ _ = Nothing

class Matcher(object):
    def __init__(self):
        self.subst = dict()
    def match(self, pat, exp):
        if isinstance(pat, Var):
            e = self.subst.get(pat.vname, None)
            if e == None:
                self.subst[pat.vname] = exp
            elif e != exp:
                self.subst = None
        elif isinstance(pat, Call) and pat.hasTheSameFunctorAs(exp):
            for p, e in itertools.izip(pat.args,exp.args):
                self.match(p, e)
                if self.subst == None:
                    return
        else:
            self.subst = None

def matchAgainst(pat, exp):
    matcher = Matcher()
    matcher.match(pat, exp);
    return matcher.subst

#mkName :: (Show a) => a -> [Char]
#
#mkName t = "v" ++ show t
#
#freshName :: State Int Name
#freshName =
#  do t <- get
#     put $ t+1
#     return $ mkName t
#
#freshNameList :: (MonadState a m, Num a, Enum a) => a -> m [String]
#
#freshNameList n =
#  do t <- get
#     put $ t + n
#     return $ map mkName [t..(t+n-1)]
