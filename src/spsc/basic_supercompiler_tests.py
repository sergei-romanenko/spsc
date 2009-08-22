'''
Created on Aug 20, 2009

@author: Sergei Romanenko
'''

import unittest

from sll_parser import pExp, pProg
from algebra import *
from basic_supercompiler import *

class BasicScpTest(unittest.TestCase):

    def drStep(self, prog, e, expected):
        self.drStep0(pProg(prog), pExp(e), expected)

    def drStep0(self, prog, e, expected):
        trBuilder = DrivingEngine(NameGen("v", 10000), prog)
        branches = trBuilder.drivingStep(e)
        branches_s = "".join(["(%s,%s)" % (exp, contr) for exp, contr in branches])
        self.assertEqual(expected, branches_s)

    def buildPrTree1(self, prog, e):
        nameGen = NameGen("v", 10000)
        return buildBasicProcessTree(nameGen, 1, prog, e)

    def buildPrTree1Test(self, prog, e):
        return self.buildPrTree1(pProg(prog), pExp(e))

    def buildPrTree1OK(self, prog, e, expected):
        tree = self.buildPrTree1(pProg(prog), pExp(e))
        tree_s = "%s" % tree
        self.assertEqual(expected, tree)

#
#buildPrTree1Adv prog e =
#  (evalState $ buildStart advanced_buildStep prog (initTree e)) 10000 
#
#testBPT1Adv prog e =
#  buildPrTree1Adv (pProg prog) (pExp e)
#
    pAdd = "gAdd(Z,y)=y;gAdd(S(x),y)=S(gAdd(x,y));"
    pAddAcc = "gAddAcc(Z,y)=y;gAddAcc(S(x),y)=gAddAcc(x,S(y));"

    def testCtr(self):
        "Ctr"
        self.drStep("", "C(a,b)", "(a,None)(b,None)")

    def testFCall(self):
        "FCall"
        self.drStep("f(x)=x;", "f(A(z))", "(A(z),None)")

    def testGCallCtr(self):
        "GCallCtr"
        self.drStep(self.pAddAcc,
                    "gAddAcc(S(S(Z)), Z)",
                    "(gAddAcc(S(Z),S(Z)),None)")

    def testGCallVar(self):
        "GCallVar"
        self.drStep(self.pAddAcc,
                    "gAddAcc(a,b)",
                    "(b,a = Z)(gAddAcc(v10000,S(b)),a = S(v10000))")

    def testGCallGeneral(self):
        "GCallGeneral"
        self.drStep(self.pAddAcc,
                    "gAddAcc(gAddAcc(a,b),c)",
                    "(gAddAcc(b,c),a = Z)(gAddAcc(gAddAcc(v10000,S(b)),c),a = S(v10000))")

    def testLet(self):
        "Let"
        self.drStep0(Program([]),
                     Let(Ctr("C", [Var("x"), Var("y")]), [("x", Var("a")), ("y", Var("b"))]),
                     "(C(x,y),None)(a,None)(b,None)")

#    def testPrTrVar(self):
#        "PrTrCtr"
#        self.buildPrTree1OK("", "x", "")

#    def testBasicScp(self):
#        # testPrTrCtr 
#        print self.buildPrTree1Test("", "S(Z)")
#        # testAdd1_0
#        print self.buildPrTree1Test(self.pAddAcc, "gAddAcc(S(Z), Z)")
#        # testAddAB 
#        print self.buildPrTree1Test(self.pAdd, "gAdd(a, b)")
#        # testAddAdd 
#        print self.buildPrTree1Test(self.pAdd, "gAdd(gAdd(a,b),c)")
#        return

#testAPTVar = testBPT1Adv "" "x"
#testAPTCtr = testBPT1Adv "" "S(Z)"
#testAAdd1_0 = testBPT1Adv pAddAcc "gAddAcc(S(Z), Z)"
#testAAddAB = testBPT1Adv pAdd "gAdd(a, b)"
#testAAddAdd = testBPT1Adv pAdd "gAdd(gAdd(a,b),c)"
