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
        self.cparams = cparams

    def __str__(self):
        cparams_s = ",".join(self.cparams)
        pat_s = self.cname
        if len(self.cparams) > 0 :
            pat_s += "(" + cparams_s + ")"
        return self.vname + " = " + pat_s

def showNodeId(node):
    if node :
        return node.nodeId
    else:
        return None

class Node(object):
    "The constructor is supposed to be called via ProcessTree#newNode only."
    
    def __init__(self, tree, exp, contr, parent, children):
        "nodeId is only used for unit testing purposes"
        self.nodeId = tree.getFreshNodeId()
        self.exp = exp
        self.contr = contr
        self.parent = parent
        self.children = children

    def __str__(self):
        children_s = ",".join(["%s" % n.nodeId for n in self.children])
        return "%s:(%s,%s,%s,[%s])" % (self.nodeId, self.exp,
                                     self.contr, showNodeId(self.parent),
                                     children_s)

    def ancestors(self):
        n = self.parent
        while n:
            yield n
            n = n.parent

    def funcAncestor(self):
        for n in  self.ancestors():
            if equiv(self.exp, n.exp):
                return n
        return None

    def findMoreGeneralAncestor(self):
        for n in self.ancestors():
            if n.exp.isFGCall() and instOf(self.exp, n.exp):
                return n
        return None

    def isProcessed(self):
        if self.exp.isVar():
            return True
        elif self.exp.isCtr():
            return self.exp.args == []
        elif self.exp.isFGCall():
            return self.funcAncestor() != None
        elif self.exp.isLet():
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

    def __init__(self, exp):
        self.freshNodeId = -1
        self.root = self.newNode(exp, None, None, [])
        
    def __str__(self):
        nodes_s = ",".join(["%s" % n for n in self.nodes()])
        return "{%s}" % nodes_s

    def getFreshNodeId(self):
        self.freshNodeId += 1
        return self.freshNodeId

    def newNode(self, exp, contr, parent, children):
        return Node(self, exp, contr, parent, children)

    def nodes(self):
        for n in self.root.subtreeNodes():
            yield n

    def leaves(self):
        "Here, the tree is supposed not to be empty."
        for n in self.root.subtreeLeaves():
            yield n

    def findUnprocessedNode(self):
        for n in self.leaves():
            if not n.isProcessed():
                return n
        return None

    def isFuncNode(self, node):
        for leaf in self.leaves():
            if node == leaf.funcAncestor():
                return True
        return False

    def addChildren(self, node, branches):
        children = [self.newNode(exp, contr, node, []) for (exp, contr) in branches]
        node.children += children

    def replaceSubtree(self, node, exp):
        node.children = []
        node.exp = exp
