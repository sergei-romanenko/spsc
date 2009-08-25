'''
Created on Aug 18, 2009

@author: Sergei Romanenko
'''

import unittest

from sll_language import *
from process_tree import *

class ProcessTreeTest(unittest.TestCase):

    def setUp(self):
        t = ProcessTree(Var("r"))
        r = t.root
        t.addChildren(r,[(Var("m1"), None), (Var("m2"), None)])
        m1 = r.children[0]
        m2 = r.children[1]
        t.addChildren(m1, [(Var("n"), None)])
        t.replaceSubtree(m2, Var("x"))
        self.tree = t

    def test01PrTreeBuilding(self):
        "PrTreeBuilding"
        self.assertEqual("{0:(r,None,None,[1,2]),1:(m1,None,0,[3]),3:(n,None,1,[]),2:(x,None,0,[])}",
                         "%s" % self.tree)

    def test02PrTreeNodes(self):
        "PrTreeNodes"
        self.assertEqual([0,1,3,2], [n.nodeId for n in self.tree.nodes()])

    def test03PrTreeLeaves(self):
        "PrTreeLeaves"
        self.assertEqual([3,2], [n.nodeId for n in self.tree.leaves()])
