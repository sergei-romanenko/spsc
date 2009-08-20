'''
Created on Aug 17, 2009

@author: Sergei Romanenko
'''

from sll_language import *
from algebra import *

class Contraction(object):
    def __init__(self, vname, cname, cparams):
        self.vname = vname
        self.cname = cname
        self,cparams = cparams

    def __str__(self):
        cparams_s = ",".join(self.cparams)
        pat_s = self.cname
        if len(self.cparams) > 0 :
            pat_s += "(" + cparams_s + ")"
        return self.vname + " = " + pat_s

def showNodeId(node):
    if node :
        return id(node)
    else:
        return None

class Node(object):
    def __init__(self, exp, contr, parent, children):
        self.exp = exp
        self.contr = contr
        self.parent = parent
        self.children = children

    def __str__(self):
        children_s = ",".join(["%s" % id(n) for n in self.children])
        return "%s:(%s,%s,%s,[%s])" % (id(self), self.exp,
                                     self.contr, showNodeId(self.parent),
                                     children_s)

    def ancestors(self):
        n = self.parent
        while n:
            yield n
            n = n.parent

    def funcAncestor(self):
        for n in  ancestors:
            if equiv(self.exp, n.exp):
                return n
        return None

    def isProcessed(self):
        if isinstance(self.exp, Var):
            return True
        elif isInstance(self.exp, Ctr):
            return self.exp.args == []
        elif isInstance(self.exp, FCall) or isInstance(self.exp, GCall):
            return self.funcAncestor() != None
        elif isInstance(self.exp, Let):
            return False
        else:
            raise ValueError("Invalid exp")

    def subtreeNodes(self):
        yield self
        for child in self.children:
            for n in child.subtreeNodes():
                yield n

    def isLeaf(self):
        return self.children == []

    def subtreeLeaves(self):
        if self.isLeaf():
            yield self
            return
        for child in self.children:
            for n in child.subtreeLeaves():
                yield n

class ProcessTree(object):
    "NB: The tree is not functional, since its nodes are updated in place."

    def __init__(self, root=None):
        self.root = root
        
    def __str__(self):
        if not self.root:
            return "{}"
        nodes_s = ",".join(["%s" % n for n in self.nodes()])
        return "{%s}" % nodes_s

    def nodes(self):
        if not self.root:
            return
        else:
            for n in self.root.subtreeNodes():
                yield n

    def leaves(self):
        "Here, the tree is supposed not to be empty."
        for n in self.root.subtreeLeaves():
            yield n

    def isFuncNode(self, node):
        for leaf in self.leaves():
            if node == leaf.funcAncestor():
                return True
        return False

    def addChildren(self, node, branches):
        node.children += branches

    def replaceSubtree(selfself, exp):
        node.children = []
        node.exp = exp
