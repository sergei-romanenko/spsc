'''
Created on Aug 17, 2009

@author: Sergei Romanenko
'''

import itertools

from sll_language import *

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
        elif (isinstance(pat, Call) and
              pat.hasTheSameFunctorAs(exp) and
              len(pat.args) == len(exp.args)):
            for p, e in itertools.izip(pat.args, exp.args):
                self.match(p, e)
                if self.subst == None:
                    return
        else:
            self.subst = None

def matchAgainst(pat, exp):
    matcher = Matcher()
    matcher.match(pat, exp);
    return matcher.subst

def instOf(e1, e2):
    return matchAgainst(e2, e1) != None

def equiv(e1, e2):
    return instOf(e1, e2) and instOf(e2, e1)

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
