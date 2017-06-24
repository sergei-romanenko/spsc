'''
Created on Aug 14, 2009

@author: Sergei Romanenko
'''

import unittest

from pyparsing import stringEnd, ParseException

from sll_language import *
from sll_parser import *

class SLLParser_Tests(unittest.TestCase):

    def stringOK(self, p, expected, input):
        tokens = p.parseString(input, True)
        self.assertEqual(expected, tokens[0])

    def parseOK(self, p, expected, input):
        tokens = p.parseString(input, True)
        self.assertEqual(expected, "%s" % tokens[0])

    def expOK(self, expected, input):
        self.parseOK(exp, expected, input)

    def programOK(self, expected, input):
        self.parseOK(program, expected, input)

    def exc(self, p, input):
        self.assertRaises(ParseException, p.parseString, input, True)

    def testIdents(self):
        self.stringOK(lIdent, "aAa9b", "aAa9b")
        self.stringOK(uIdent, "Aaa9b", "Aaa9b")
        self.stringOK(fIdent, "fAa9b", "fAa9b")
        self.stringOK(gIdent, "gAa9b", "gAa9b")
        self.exc(lIdent, "Aa")
        self.exc(uIdent, "aa")
        self.exc(lIdent, "A*a")

    def testCall(self):
        self.expOK("x", "x")
        self.expOK("fA", "fA")
        self.expOK("gA", "gA")
        self.expOK("A(x,y)", "A(x,y)")
        self.expOK("A", "A()")
        self.expOK("A", "A")
        self.expOK("fA(x,y)", "fA(x,y)")
        self.expOK("fA()", "fA()")
        self.expOK("gA(x,y)", "gA(x,y)")
        self.exc(exp, "gA()")

    def testProgram(self):
        self.programOK("", "")
        self.programOK("f(x)=A;", "f(x)=A;")
        self.programOK("f(x)=A;f1(x)=A1;", "f(x)=A;f1(x)=A1;")
        self.programOK("f()=f();", "f()=f();")
        self.programOK("g(C)=A;", "g(C)=A;")
        self.programOK("g(C(x))=A;", "g(C(x))=A;")
        self.programOK("g(C(x))=A;", "g(C(x))=A;")
        self.programOK("g(C(x,y),z)=A;", "g(C(x,y),z)=A;")
        self.programOK("g(C,y)=A;", "g(C,y)=A;")


if __name__ == "__main__":
    #import sys;sys.argv = ['', 'Test.testName']
    unittest.main()
