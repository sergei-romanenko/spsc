'''
Created on Aug 25, 2009

@author: Sergei Romanenko
'''

# MSG = Most Specific Generalization

from sll_language import *
from algebra import *

class Gen(object):

    def __init__(self, exp, subst1, subst2):
        self.exp = exp
        self.subst1 = subst1
        self.subst2 = subst2

    def __str__(self):
        assoc1 = list(self.subst1.items())
        assoc1.sort()
        assoc1_s = ",".join(["%s=%s" % a for a in assoc1])
        assoc2 = list(self.subst2.items())
        assoc2.sort()       
        assoc2_s = ",".join(["%s=%s" % a for a in assoc2])
        return "%s =>> {%s}{%s}" % (self.exp, assoc1_s, assoc2_s)

class MSGBuilder(object):

    def __init__(self, nameGen):
        self.nameGen = nameGen
        self.exp = None
        self.subst = None
        self.noProgress = None

    def build(self, e1, e2):
        vname = self.nameGen.freshName()
        self.exp = Var(vname)
        self.subst = dict([(vname, (e1, e2))])
        while True:
            self.noProgress = True
            self.mergeSubexp()
            if self.noProgress:
                self.commonFunctor()
            if self.noProgress:
                break
        subst1 = dict()
        subst2 = dict()
        for (vname, (e1, e2)) in self.subst.items():
            subst1[vname] = e1
            subst2[vname] = e2
        result = Gen(self.exp, subst1, subst2)
        self.exp = None
        self.subst = None
        return result

    def commonFunctor(self):
        for (vname, (e1, e2)) in self.subst.items():
            if e1.hasTheSameFunctorAs(e2):
                self.noProgress = False
                ns = self.nameGen.freshNameList(len(e1.args))
                vs = [Var(x) for x in ns]
                self.exp = self.exp.applySubst({vname : e1.cloneFunctor(vs)})
                del self.subst[vname]
                self.subst.update(zip(ns, zip(e1.args, e2.args)))
                return

    def aMergeableKeyPair(self):
        for i in self.subst.keys():
            for j in self.subst.keys():
                if i < j and self.subst[i] == self.subst[j]:
                    return (i, j)
        return None               

    def mergeSubexp(self):
        ij = self.aMergeableKeyPair()
        if not ij:
            return
        i = ij[0]
        j = ij[1]
        self.noProgress = False
        self.exp = self.exp.applySubst({i : Var(j)})
        del self.subst[i]
