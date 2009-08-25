'''
Created on Aug 22, 2009

@author: Sergei Romanenko
'''

import unittest

from sll_language import *
from sll_parser import pExp, pProg
from basic_supercompiler import *
from residual_program_generator import *


class ResidualProgramGenerator_Tests(unittest.TestCase):

    # Sample programs
    pAdd = "gAdd(Z,y)=y;gAdd(S(x),y)=S(gAdd(x,y));"
    pAddAcc = "gAddAcc(Z,y)=y;gAddAcc(S(x),y)=gAddAcc(x,S(y));"

#---- Basic supercompiler

    def runBasicScp(self, prog, e):
        nameGen = NameGen("v", 100)
        tree = buildBasicProcessTree(nameGen, 100, prog, e)
        res = ResidualProgramGenerator(tree).genResidualProgram()
        return res

    def basicScpOK(self, prog, e, expected):
        (resPr, resExp) = self.runBasicScp(pProg(prog), pExp(e))
        res_s = "%s$%s" % (resPr, resExp)
        self.assertEqual(expected, res_s)

    def test101BVar(self):
        "BVar"
        self.basicScpOK("", "a", "$a")
        
    def test102BCtr(self):
        "BCtr"
        self.basicScpOK("", "C(a,b)", "$C(a,b)")

    def test103BAddAB(self):
        "BAddAB"
        self.basicScpOK(
                self.pAdd,
                "gAdd(a, b)",
                "gAdd1(Z,b)=b;gAdd1(S(v100),b)=S(gAdd1(v100,b));$gAdd1(a,b)")

    def test104BAddAdd(self):
        "BAddAdd"
        self.basicScpOK(
                self.pAdd,
                "gAdd(gAdd(a,b),c)",
                "gAdd2(Z,c)=c;gAdd2(S(v101),c)=S(gAdd2(v101,c));gAdd1(Z,b,c)=gAdd2(b,c);gAdd1(S(v100),b,c)=S(gAdd1(v100,b,c));$gAdd1(a,b,c)")

    def test105BAddAccAB(self):
        "BAddAccAB"
        self.basicScpOK(
                self.pAddAcc,
                "gAddAcc(a, b)",
                "gAddAcc1(Z,b)=b;gAddAcc1(S(v100),b)=gAddAcc1(v100,S(b));$gAddAcc1(a,b)")

#---- Advanced supercompiler
#
#advancedScpTests = TestLabel "AdvancedScp" ( TestList [
#  testAdvAddAB, testAdvAddAdd, testAdvAddAccAB, testAdvAddAccAddAcc 
#  ] )
#
#testAdvScp label prog e expected = TestCase $ assertEqual
#  label expected (show (runAdvScp (pProg prog) (pExp e)))
#
#runAdvScp prog e =
#  genResidualProgram $ advanced_buildProcessTree prog e
#
#testAdvAddAB = testAdvScp "AdvAddAB"
#  pAdd
#  "gAdd(a, b)"
#  "(gAdd1(Z,b)=b;gAdd1(S(v10000),b)=S(gAdd1(v10000,b));,gAdd1(a,b))"
#
#testAdvAddAA = testAdvScp "AdvAddAB"
#  pAdd
#  "gAdd(a, a)"
#  "(gAdd1(Z,v10006)=v10006;gAdd1(S(v10010),v10006)=S(gAdd1(v10010,v10006));,gAdd1(a,a))"
#
#testAdvAddAdd = testAdvScp "AdvAddAdd"
#  pAdd
#  "gAdd(gAdd(a,b),c)"
#  "(gAdd2(Z,c)=c;gAdd2(S(v10003),c)=S(gAdd2(v10003,c));gAdd1(Z,b,c)=gAdd2(b,c);gAdd1(S(v10000),b,c)=S(gAdd1(v10000,b,c));,gAdd1(a,b,c))"
#
#testAdvAddAccAB = testAdvScp "AdvAddAccAB"
#  pAddAcc
#  "gAddAcc(a, b)"
#  "(gAddAcc1(Z,b)=b;gAddAcc1(S(v10000),b)=gAddAcc1(v10000,S(b));,gAddAcc1(a,b))"
#
#testAdvAddAccAA = testAdvScp "AdvAddAccAB"
#  pAddAcc
#  "gAddAcc(a, a)"
#  "(gAddAcc1(Z,v10005)=v10005;gAddAcc1(S(v10009),v10005)=gAddAcc1(v10009,S(v10005));,gAddAcc1(a,a))"
#
#testAdvAddAccAddAcc = testAdvScp "AdvAddAccAddAcc"
#  pAddAcc
#  "gAddAcc(gAddAcc(a,b),c)"
#  "(gAddAcc2(Z,c)=c;gAddAcc2(S(v10003),c)=gAddAcc2(v10003,S(c));gAddAcc1(Z,b,c)=gAddAcc2(b,c);gAddAcc1(S(v10000),b,c)=gAddAcc1(v10000,S(b),c);,gAddAcc1(a,b,c))"
