'''
Created on Aug 18, 2009

@author: Sergei Romanenko
'''
import unittest
import itertools

from sll_language import *
from sll_parser import pExp
from algebra import matchAgainst

class Test(unittest.TestCase):

    def testSubst(self):
        "Subst"
        e1 = pExp("E1")
        e2 = pExp("E2")
        e = pExp("Cons(x1, Cons(x2, Cons(x3, Nil)))")
        subst = {"x1": e1, "x2": e2}
        self.assertEqual("Cons(E1,Cons(E2,Cons(x3,Nil)))",
                         "%s" % e.applySubst(subst))

    def testVars(self):
        "Vars"
        e = pExp("A(x,B(y,z),a)")
        self.assertEqual(['x', 'y', 'z', 'a'], e.vars())

#showMatchRes Nothing = "*"
#showMatchRes (Just subst) =
#  concat[ vname ++ "->" ++ show e ++ ";" | (vname, e) <- Map.toAscList subst ]
#
#testMatch label alpha beta expected = TestCase $ assertEqual
#  label expected (showMatchRes (matchAgainst (pExp alpha) (pExp beta)))

    def matchOK(self, pat, exp, expected):
        subst = matchAgainst(pExp(pat), pExp(exp))
        if subst:
            subst_l = list(subst.iteritems())
            subst_l.sort()
            subst = "".join(["%s->%s;" % (vname, e) 
                             for (vname, e) in subst_l])
        self.assertEqual(expected, subst)

    def matchNone(self, pat, exp):
        subst = matchAgainst(pExp(pat), pExp(exp))
        self.assertEqual(None, subst)

    def testMatchV_E(self):
        "MatchV_E"
        self.matchOK("x", "S(Z)", "x->S(Z);") 

    def testMatchC_V(self):
        "MatchC_V"
        self.matchNone("Z", "x")
         
    def testMatchC_C(self):
        "MatchC_C"
        self.matchOK("C(x,y)", "C(A,B)", "x->A;y->B;")
        
    def testMatchC1_C2(self): 
        "MatchC1_C2"
        self.matchNone("C(x,y)", "D(A,B)")
         
    def testMatchC_F(self):
        "MatchC_F"
        self.matchNone("C(x,y)", "f(A,B)")

    def testMatchX_X_Eq(self):
        "MatchX_X_Eq"
        self.matchOK("C(x,x)", "C(A,A)", "x->A;")

#testMatchX_X_Ne = testMatch "testMatchC_C_Ne" "C(x,x)" "C(A,B)" "*"

#
#testEquiv label e1 e2 expected = TestCase $ assertEqual
#  label expected ((pExp e1) `equiv` (pExp e2))
#

#testEquiv_yes = testEquiv "testEquiv_yes"
#  "gA(fB(x,y),C)" "gA(fB(a,b),C)" True
#

#testEquiv_no = testEquiv "testEquiv_yes"
#  "gA(fB(x,y),x)" "gA(fB(a,a),b)" False
