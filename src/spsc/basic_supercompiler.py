'''
Created on Aug 20, 2009

@author: Sergei Romanenko
'''

from sll_language import *
from algebra import matchAgainst 
from process_tree import Contraction, Node, ProcessTree

class DrivingEngine(object):

    def __init__(self, nameGen, prog):
        "The program is supposed to be correct: no duplicate definitions, etc."
        self.nameGen = nameGen
        self.fRule = dict()
        self.gRules = dict()
        self.gcRule = dict()
        for rule in prog.rules:
            name = rule.name
            if isinstance(rule, FRule):
                self.fRule[name] = rule
            elif isinstance(rule, GRule):
                if name in self.gRules:
                    "Lists are mutable!"
                    self.gRules[name].append(rule)
                else:
                    self.gRules[name] = [rule]
                self.gcRule[(rule.name, rule.cname)] = rule
            else:
                raise ValueError("Invalid rule")

    def drivingStep(self, e):
        if e.isCtr():
            return [(arg, None) for arg in e.args]
        elif e.isFCall():
            rule = self.fRule[e.name]
            p2a = dict(zip(rule.params, e.args))
            body = rule.body.applySubst(p2a)
            return [(body, None)]
        elif e.isGCall():
            arg0 = e.args[0]
            args = e.args[1:]
            if arg0.isCtr():
                cname = arg0.name
                cargs = arg0.args
                rule = self.gcRule[(e.name, cname)]
                p2a = dict()
                p2a.update(zip(rule.cparams, cargs))
                p2a.update(zip(rule.params, args))
                body = rule.body.applySubst(p2a)
                return [(body, None)]
            elif arg0.isVar():
                rules = self.gRules[e.name]
                return [ self.driveBranch(e, rule) for rule in rules ]
            else:
                branches = self.drivingStep(arg0)
                return [ (GCall(e.name, [exp] + args), c) for (exp, c) in branches]
        elif e.isLet():
            return [(e.body, None)] + [(exp, None) for (vn, exp) in e.bindings]
        else:
            raise ValueError("Unknown expression type")

    def driveBranch(self, e, rule):
        vname = e.args[0].vname
        cname = rule.cname
        cparams = self.nameGen.freshNameList(len(rule.cparams))
        params = rule.params
        cargs = [Var(vn) for vn in cparams]
        vname2ctr = dict([(vname, Ctr(cname, cargs))])
        e1 = e.applySubst(vname2ctr)
        branches = self.drivingStep(e1)
        e2 = branches[0][0]
        return (e2, Contraction(vname, cname, cparams))

class BasicProcessTreeBuilder(object):

    def __init__(self, drivingEngine, exp):
        self.drivingEngine = drivingEngine
        self.tree = ProcessTree(exp)

    # The parts common to the basic and advanced supercompilers.

    # If beta `instOf` alpha, we generalize beta by introducing
    # a let-expression, in order to make beta the same as alpha
    # (modulo variable names).

    def loopBack(self, beta, alpha):
        subst = matchAgainst(alpha.exp, beta.exp)
        bindings = list(subst)
        bindings.sort()
        letExp = Let(alpha.exp, bindings)
        self.tree.replaceSubtree(beta, letExp)

    # This function applies a driving step to the node's expression,
    # and, in general, adds children to the node.

    def expandNode(self, beta):
        branches = self.drivingEngine.drivingStep(beta.exp)
        self.tree.addChildren(beta, branches)

    # Basic supercompiler process tree builder

    def buildStep(self, beta):
        """
        This method is overridden in the advanced version of
        the process tree builder.
        """
        alpha = beta.findMoreGeneralAncestor()
        if alpha:
            self.loopBack(beta, alpha)
        else:
            self.expandNode(beta)

    def buildProcessTree(self, k):
        # Specifying k = -1 results in an unlimited building loop.
        while True:
            if k == 0:
                break
            k -= 1
            beta = self.tree.findUnprocessedNode()
            if not beta:
                break
            self.buildStep(beta)

def buildBasicProcessTree(nameGen, k, prog, exp):
    drivingEngine = DrivingEngine(nameGen, prog)
    builder = BasicProcessTreeBuilder(drivingEngine, exp)
    builder.buildProcessTree(k)
    return builder.tree