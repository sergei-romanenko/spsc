'''
Created on Aug 14, 2009

@author: Sergei Romanenko
'''

import unittest

from sll_language import *

class SLanguage_Tests(unittest.TestCase):

    def testStrCall(self):
        self.assertEqual("C(x,y)", "%s" % Call(1, "C", [Var("x"), Var("y")]))
        self.assertEqual("C(x,y)", "%s" % Ctr("C", [Var("x"), Var("y")]))
        self.assertEqual("f(x,y)", "%s" % FCall("f", [Var("x"), Var("y")]))
        self.assertEqual("g(x,y)", "%s" % GCall("g", [Var("x"), Var("y")]))

    def testStrLet(self):
        self.assertEqual("let x=a in x", "%s" % Let(Var("x"), [Binding("x", Var("a"))]))

    def testTheSameFunctor(self):
        self.assertTrue(Ctr("A", []).hasTheSameFunctorAs(Ctr("A", [])))
        self.assertTrue(FCall("A", []).hasTheSameFunctorAs(FCall("A", [])))
        self.assertTrue(GCall("A", []).hasTheSameFunctorAs(GCall("A", [])))
        self.assertTrue(not Ctr("A", []).hasTheSameFunctorAs(Ctr("B", [])))
        self.assertTrue(not Ctr("A", []).hasTheSameFunctorAs(FCall("A", [])))

    def testFCall(self):
        self.assertEqual("f(x,y)=x;", "%s" % FRule("f", ["x", "y"], Var("x")))

    def testProgram(self):
        self.assertEqual("f()=A;f1()=A1;", "%s" % \
                         Program([FRule("f", [], Ctr("A",[])),
                                 FRule("f1", [], Ctr("A1",[]))]))
        self.assertEqual("g(C)=A;g1(C,x)=A;g2(C(x))=A;", "%s" % \
                         Program([GRule("g", "C", [], [], Ctr("A",[])),
                                 GRule("g1", "C", [], ["x"], Ctr("A",[])),
                                 GRule("g2", "C", ["x"], [], Ctr("A",[]))]))
    def eqCheck(self, e1, e2):
        self.assertTrue(e1 == e2)

    def neCheck(self, e1, e2):
        self.assertTrue(e1 != e2)

    def testEq(self):
        self.eqCheck(Var("x"), Var("x"))
        self.neCheck(Var("x"), Var("y"))
        self.eqCheck(Ctr("A", []), Ctr("A", []))
        self.neCheck(Ctr("A", []), Ctr("B", []))
        self.eqCheck([], [])
        self.eqCheck([Var("x")], [Var("x")])
        self.neCheck([Var("x")], [Var("y")])
        self.neCheck([Var("x")], [Var("x"), Var("z")])
        self.eqCheck(Ctr("A", [Var("x")]), Ctr("A", [Var("x")]))
        self.neCheck(Ctr("A", [Var("x")]), Ctr("A", [Var("y")]))

if __name__ == "__main__":
    #import sys;sys.argv = ['', 'Test.testName']
    unittest.main()
