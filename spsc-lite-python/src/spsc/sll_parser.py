'''
Created on Aug 14, 2009

@author: Sergei Romanenko
'''

import string
from pyparsing import Word, Optional, delimitedList, alphanums, Literal, Suppress, \
    Group, Forward, ZeroOrMore, OneOrMore, line, col, ParseException, stringEnd

from sll_language import *

def mkIdent(tokens):
    return tokens[0]

def mkVar(t):
    return Var(t[0][0])

lIdent = Word(string.lowercase, alphanums)
lIdent.setParseAction(mkIdent)

uIdent = Word(string.uppercase, alphanums)
uIdent.setParseAction(mkIdent)

fIdent = Word("f", alphanums)
fIdent.setParseAction(mkIdent)

gIdent = Word("g", alphanums)
gIdent.setParseAction(mkIdent)

LPAR = Suppress("(")
RPAR = Suppress(")")
EQ = Suppress("=")
SC = Suppress(";")

exp = Forward();

variable = Group(lIdent)
variable.setParseAction(mkVar)

ctrArgList = Group(Optional(LPAR + Optional(delimitedList(exp)) + RPAR))

constructor = uIdent + ctrArgList
constructor.setParseAction(lambda t: Ctr(t[0], list(t[1])))

fArgList = Group(LPAR + Optional(delimitedList(exp)) + RPAR)

fCall = fIdent + fArgList
fCall.setParseAction(lambda t: FCall(t[0], list(t[1])))


gArgList = Group(LPAR + delimitedList(exp) + RPAR)

gCall = gIdent + gArgList
gCall.setParseAction(lambda t: GCall(t[0], list(t[1])))

exp << (constructor | fCall | gCall | variable)

patternArgList = Group(Optional(LPAR + Optional(delimitedList(lIdent)) + RPAR))

pattern = uIdent + patternArgList

fParamList = Group(Optional(delimitedList(lIdent)))

fRule = fIdent + LPAR + fParamList + RPAR + EQ + exp + SC
fRule.setParseAction(lambda t: FRule(t[0], list(t[1]), t[2]))

gParamList = Group(Optional(Suppress(",") + delimitedList(lIdent)))

gRule = gIdent + LPAR + pattern + gParamList + RPAR + EQ + exp + SC
gRule.setParseAction(lambda t: GRule(t[0], t[1], list(t[2]), list(t[3]), t[4]))

program = ZeroOrMore(fRule | gRule)
program.setParseAction(lambda t: Program(list(t)))

def pExp(input):
    return exp.parseString(input, True)[0]

def pProg(input):
    return program.parseString(input, True)[0]