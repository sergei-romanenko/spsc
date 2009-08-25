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
        trBuilder = DrivingEngine(NameGen("v", 100), prog)
        branches = trBuilder.drivingStep(e)
        branches_s = "".join(["(%s,%s)" % (exp, contr) for exp, contr in branches])
        self.assertEqual(expected, branches_s)

    def buildPrTree1(self, prog, e):
        nameGen = NameGen("v", 100)
        return buildBasicProcessTree(nameGen, 1, prog, e)

    def buildPrTree1OK(self, prog, e, expected):
        tree = self.buildPrTree1(pProg(prog), pExp(e))
        self.assertEqual(expected, "%s" % tree)

    def buildPrTree(self, prog, e):
        nameGen = NameGen("v", 100)
        return buildBasicProcessTree(nameGen, 100, prog, e)

    def buildPrTreeOK(self, prog, e, expected):
        tree = self.buildPrTree(pProg(prog), pExp(e))
        self.assertEqual(expected, "%s" % tree)

#
#buildPrTree1Adv prog e =
#  (evalState $ buildStart advanced_buildStep prog (initTree e)) 10000 
#
#testBPT1Adv prog e =
#  buildPrTree1Adv (pProg prog) (pExp e)
#
    pAdd = "gAdd(Z,y)=y;gAdd(S(x),y)=S(gAdd(x,y));"
    pAddAcc = "gAddAcc(Z,y)=y;gAddAcc(S(x),y)=gAddAcc(x,S(y));"

    def test101Ctr(self):
        "Ctr"
        self.drStep("", "C(a,b)", "(a,None)(b,None)")

    def test102FCall(self):
        "FCall"
        self.drStep("f(x)=x;", "f(A(z))", "(A(z),None)")

    def test103GCallCtr(self):
        "GCallCtr"
        self.drStep(
            self.pAddAcc,
            "gAddAcc(S(S(Z)), Z)",
            "(gAddAcc(S(Z),S(Z)),None)")

    def test104GCallVar(self):
        "GCallVar"
        self.drStep(
            self.pAddAcc,
            "gAddAcc(a,b)",
            "(b,a=Z)(gAddAcc(v100,S(b)),a=S(v100))")

    def test105GCallGeneral(self):
        "GCallGeneral"
        self.drStep(
            self.pAddAcc,
            "gAddAcc(gAddAcc(a,b),c)",
            "(gAddAcc(b,c),a=Z)(gAddAcc(gAddAcc(v100,S(b)),c),a=S(v100))")

    def test106Let(self):
        "Let"
        self.drStep0(
             Program([]),
             Let(Ctr("C", [Var("x"), Var("y")]), [("x", Var("a")), ("y", Var("b"))]),
             "(C(x,y),None)(a,None)(b,None)")

    def test201PrTrVar1(self):
        "PrTrVar1"
        self.buildPrTree1OK(
            "", "x",
            "{0:(x,None,None,[])}")

    def test201PrTrCtr1(self):
        "PrTrCtr1"
        self.buildPrTree1OK(
            "", "S(Z)",
            "{0:(S(Z),None,None,[1]),1:(Z,None,0,[])}")

    def test202AddS_Z1(self):
        "AddS_Z1"
        self.buildPrTree1OK(
            self.pAddAcc, "gAddAcc(S(Z), Z)",
            "{0:(gAddAcc(S(Z),Z),None,None,[1]),1:(gAddAcc(Z,S(Z)),None,0,[])}")

    def test203AddAB1(self):
        "AddAB1"
        self.buildPrTree1OK(
            self.pAdd, "gAdd(a, b)",
            "{0:(gAdd(a,b),None,None,[1,2]),1:(b,a=Z,0,[]),2:(S(gAdd(v100,b)),a=S(v100),0,[])}")

    def test204AddAdd1(self):
        "AddAdd1" 
        self.buildPrTree1OK(
            self.pAdd, "gAdd(gAdd(a,b),c)",
            "{0:(gAdd(gAdd(a,b),c),None,None,[1,2]),1:(gAdd(b,c),a=Z,0,[]),2:(gAdd(S(gAdd(v100,b)),c),a=S(v100),0,[])}")

    def test205AddAccAB(self):
        "AddAccAB" 
        self.buildPrTreeOK(
           self.pAddAcc, "gAddAcc(a,b)",
           "{0:(gAddAcc(a,b),None,None,[1,2]),1:(b,a=Z,0,[]),2:(let a=v100,b=S(b) in gAddAcc(a,b),a=S(v100),0,[3,4,5]),3:(gAddAcc(a,b),None,2,[]),4:(v100,None,2,[]),5:(S(b),None,2,[6]),6:(b,None,5,[])}")

    def test301AddS_Z(self):
        "AddS_Z"
        self.buildPrTreeOK(
           self.pAddAcc, "gAddAcc(S(Z), Z)",
            "{0:(gAddAcc(S(Z),Z),None,None,[1]),1:(gAddAcc(Z,S(Z)),None,0,[2]),2:(S(Z),None,1,[3]),3:(Z,None,2,[])}")

    def test303AddAB(self):
        "AddAB"
        self.buildPrTreeOK(
           self.pAdd, "gAdd(a, b)",
           "{0:(gAdd(a,b),None,None,[1,2]),1:(b,a=Z,0,[]),2:(S(gAdd(v100,b)),a=S(v100),0,[3]),3:(gAdd(v100,b),None,2,[])}")

    def test304AddAdd(self):
        "AddAdd" 
        self.buildPrTreeOK(
           self.pAdd, "gAdd(gAdd(a,b),c)",
            "{0:(gAdd(gAdd(a,b),c),None,None,[1,2]),1:(gAdd(b,c),a=Z,0,[3,4]),3:(c,b=Z,1,[]),4:(S(gAdd(v101,c)),b=S(v101),1,[5]),5:(gAdd(v101,c),None,4,[]),2:(gAdd(S(gAdd(v100,b)),c),a=S(v100),0,[6]),6:(S(gAdd(gAdd(v100,b),c)),None,2,[7]),7:(gAdd(gAdd(v100,b),c),None,6,[])}")

#testAPTVar = testBPT1Adv "" "x"
#testAPTCtr = testBPT1Adv "" "S(Z)"
#testAAdd1_0 = testBPT1Adv pAddAcc "gAddAcc(S(Z), Z)"
#testAAddAB = testBPT1Adv pAdd "gAdd(a, b)"
#testAAddAdd = testBPT1Adv pAdd "gAdd(gAdd(a,b),c)"
