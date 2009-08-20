'''
Created on Aug 20, 2009

@author: Sergei Romanenko
'''

import unittest

from sll_parser import pExp, pProg
from algebra import *
from basic_supercompiler import *

#p = DrivingEngine(pProg("f(x)=x;g(A)=A;g(B)=B;"))
#
#y=1
class BasicScpTest(unittest.TestCase):

    def drStep(self, prog, e, expected):
        drEngine = DrivingEngine(pProg(prog), NameGen("v", 10000))
        branches = drEngine.drivingStep(pExp(e))
        branches_s = "".join(["(%s,%s)" % (exp, contr) for exp, contr in branches])
        self.assertEqual(expected, branches_s)

#testBuildPrTree prog e =
#  basic_buildProcessTree (pProg prog) (pExp e)
#
#buildStart buildStep prog tree =
#  case unprocessedNodes tree of
#    [] -> return tree
#    beta : _ ->
#      do tree' <- buildStep prog tree beta
#         return tree'
#
#buildPrTree1 prog e =
#  (evalState $ buildStart basic_buildStep prog (initTree e)) 10000 
#
#testBuildPrTree1 prog e =
#  buildPrTree1 (pProg prog) (pExp e)
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

#testGCallGeneral = testDrStep
#  pAddAcc
#  "GCallGeneral"
#  "gAddAcc(gAddAcc(a, b), c)"
#  "[(gAddAcc(b,c),Just a = Z),(gAddAcc(gAddAcc(v100,S(b)),c),Just a = S(v100))]"
#
#testLet = testDrStep'
#  (Program [])
#  "Let"
#  (Let (Call Ctr "C" [Var "x", Var "y"]) [("x", Var "a"), ("y", Var "b")])
#  "[(C(x,y),Nothing),(a,Nothing),(b,Nothing)]"
#
#testPrTrVar = testBuildPrTree "" "x"
#testPrTrCtr = testBuildPrTree "" "S(Z)"
#testAdd1_0 = testBuildPrTree pAddAcc "gAddAcc(S(Z), Z)"
#testAddAB = testBuildPrTree pAdd "gAdd(a, b)"
#testAddAdd = testBuildPrTree pAdd "gAdd(gAdd(a,b),c)"
#
#testAPTVar = testBPT1Adv "" "x"
#testAPTCtr = testBPT1Adv "" "S(Z)"
#testAAdd1_0 = testBPT1Adv pAddAcc "gAddAcc(S(Z), Z)"
#testAAddAB = testBPT1Adv pAdd "gAdd(a, b)"
#testAAddAdd = testBPT1Adv pAdd "gAdd(gAdd(a,b),c)"
