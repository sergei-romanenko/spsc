'''
Created on Aug 17, 2009

@author: Sergei Romanenko
'''

import unittest

from sll_language import *
from sll_parser import pExp, pProg
from he import embeddedIn, aVarIsUnderAttack, he

class HE_Tests(unittest.TestCase):

    def heTrue(self, input1, input2):
        e1 = pExp(input1)
        e2 = pExp(input2)
        self.assertTrue(he(e1, e2))

    def heFalse(self, input1, input2):
        e1 = pExp(input1)
        e2 = pExp(input2)
        self.assertFalse(he(e1, e2))

    def varAttackTrue(self, input):
        e = pExp(input)
        self.assertTrue(aVarIsUnderAttack(e))

    def varAttackFalse(self, input):
        e = pExp(input)
        self.assertFalse(aVarIsUnderAttack(e))

    def testVarAttack(self):
        self.varAttackTrue("x")
        self.varAttackFalse("A")
        self.varAttackFalse("f(x)")
        self.varAttackTrue("g(x,y)")
        self.varAttackTrue("g1(g2(x))")
        self.varAttackFalse("g(A)")
        self.varAttackFalse("g(f(x))")

    def testVV(self):
        "v1 <| v2"
        self.heTrue("v1", "v2")

    def testVF(self):
        "v1 <| F(v2)"
        self.heTrue("v1", "F(v2)")

    def testFV(self):
        "not F(v2) <| v1"
        self.heFalse("F(v2)", "v1")

    def testDiving(self):
        "F(v1) < G(v0, F(G(v2)))"
        self.heTrue("F(v1)", "G(v0, F(H(v2)))")

    def testCoupling1(self):
        "F(v1, G(v2)) <| F(H(w1), G(w2))"
        self.heTrue("F(v1, G(v2))", "F(H(w1), G(w2))")

    def testCoupling2(self):
        "not f(v1) <| g(w1)"
        self.heFalse("f(v1)", "g(w1)")
