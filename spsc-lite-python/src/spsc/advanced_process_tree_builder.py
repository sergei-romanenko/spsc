'''
Created on Aug 26, 2009

@author: Sergei Romanenko
'''

### Advanced Supercompiler with homeomorphic imbedding and generalization  

from basic_process_tree_builder import *
from he import embeddedIn
from msg import MSGBuilder

class AdvancedProcessTreeBuilder(BasicProcessTreeBuilder):

    def __init__(self, drivingEngine, exp):
        self.drivingEngine = drivingEngine
        self.tree = ProcessTree(exp)
        self.nameGen = drivingEngine.nameGen
        self.msgBuilder = MSGBuilder(self.nameGen)

    def abstract(self, alpha, exp, subst):
        bindings = list(subst.items())
        bindings.sort()
        letExp = Let(exp, bindings)
        self.tree.replaceSubtree(alpha, letExp)

    def split(self, beta):
        exp = beta.exp
        args = exp.args
        names1 = self.nameGen.freshNameList(len(args))
        args1 = [Var(x) for x in names1]
        letExp = Let(beta.e.cloneFunctor(args1), zip(names1, args))
        self.tree.replaceSubtree(beta, letExp)

    def generalizeAlphaOrSplit(self, beta, alpha):
        gen = self.msgBuilder.build(alpha.exp, beta.exp)
        if gen.exp.isVar():
            self.split(beta)
        else:
            self.abstract(alpha, gen.exp, gen.substA)

    def findEmbeddedAncestor(self, beta):
        for alpha in beta.ancestors():
            if alpha.exp.isFGCall() and embeddedIn(alpha.exp, beta.exp):
                return alpha
        return None

    def buildStep(self, beta):
        """
        This method overrides the method in the basic version of
        the process tree builder.
        """
        alpha = beta.findMoreGeneralAncestor()
        if alpha:
            self.loopBack(beta, alpha)
        else:
            alpha = self.findEmbeddedAncestor(beta)
            if alpha:
                self.generalizeAlphaOrSplit(beta, alpha)
            else:
                self.expandNode(beta)

def buildAdvancedProcessTree(nameGen, k, prog, exp):
    drivingEngine = DrivingEngine(nameGen, prog)
    builder = AdvancedProcessTreeBuilder(drivingEngine, exp)
    builder.buildProcessTree(k)
    return builder.tree
