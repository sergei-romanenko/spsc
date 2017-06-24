'''
Created on Aug 22, 2009

@author: Sergei Romanenko
'''

from sll_language import *
from algebra import *
from process_tree import *

class ResidualProgramGenerator(object):

    def __init__(self, tree):
        self.tree = tree
        self.sigs = dict()
        self.rules = []

    def genResidualProgram(self):
        resExp = self.genExp(self.tree.root)
        return (Program(self.rules), resExp)

    def genExp(self, beta):
        alpha = beta.funcAncestor()
        exp = beta.exp
        if not alpha:
            if exp.isVar():
               return exp
            elif exp.isCtr():
                resExps = self.genExpList(beta.children)
                return Ctr(exp.name, resExps)
            elif exp.isFGCall():
                return self.genCall(beta)
            elif exp.isLet():
                resExpList = self.genExpList(beta.children)
                vnames = [ b[0] for b in exp.bindings]
                subst = dict(zip(vnames, resExpList[1:]))
                return resExpList[0].applySubst(subst)
            else:
                raise ValueError("Invalid expression")
        else:
            (name, params) = self.sigs[alpha]
            args = [Var(param) for param in params]
            subst = matchAgainst(alpha.exp, beta.exp)
            contr = alpha.children[0].contr
            if not contr:
                return FCall(name, args).applySubst(subst)
            else:
                return GCall(name, args).applySubst(subst)

    def genExpList(self, nodes):
        resExpList = []
        for node in nodes:
            resExp = self.genExp(node)
            resExpList.append(resExp)
        return resExpList

    def isVarTest(self, beta):
        if beta.children[0].contr:
            return True
        else:
            return False

    def getFGSig(self, prefix, beta, name, vs):
        sig = self.sigs.get(beta, None)
        if sig:
            return sig
        else:
            name1 = "%s%s%s" % (prefix, name[1:], len(self.sigs) + 1)
            sig1 = (name1, vs)
            self.sigs[beta] = sig1
            return sig1

    def getChContr(self, children):
        return [(n.contr.cname, n.contr.cparams) for n in children]

    def genCall(self, beta):
        exp = beta.exp
        name = exp.name
        args = exp.args
        params = exp.vars()
        if self.isVarTest(beta):
            (name1, vs1) = self.getFGSig("g", beta, name, params)
            bodies = self.genExpList(beta.children)
            contrs = self.getChContr(beta.children)
            grules = [GRule(name1, cname1, cparams1, params[1:], body1)
                      for ((cname1, cparams1), body1) in zip(contrs, bodies)]
            self.rules.extend(grules)
            return GCall(name1, [Var(param) for param in params])
        elif self.tree.isFuncNode(beta):
            (name1, vs1) = self.getFGSig("f", beta, name, params)
            body1 = self.genExp(beta.children[0])
            self.rules.append(FRule(name1, params1, body1))
            return FCall(name1, [Var(param) for param in params])
        else:
            return self.genExp(beta.children[0])
