import string
from pyparsing import Word, Optional, delimitedList, alphanums, Literal, Suppress, \
    Group, Forward, OneOrMore, line, col, ParseException

class Term(object):
    def __init__(self, variable=None, constructor=None, call=None):
        self.variable = variable
        self.constructor = constructor
        self.call = call
    def __repr__(self):
        if self.variable: return str(self.variable)
        if self.constructor: return str(self.constructor)
        if self.call: return str(self.call)
        
class Fun(object):
    def __init__(self, fFun=None, gFun=None):
        self.fFun = fFun
        self.gFun = gFun
    def __repr__(self):
        if self.fFun:return str(self.fFun)
        if self.gFun:return str(self.gFun)

class Variable(object):
    def __init__(self, name, loc):
        self.name = name
        self.loc = loc
    def __repr__(self):
        return "Variable(" + self.name + ")"
        
def createTermVar(s, loc, toks):
    return Term(variable=Variable(toks[0], loc))

def createPureVar(s, loc, toks):
    return Variable(toks[0], loc)

class Pattern(object):
    def __init__(self, name, args, loc):
        self.name = name
        self.args = args
        self.loc = loc
    def __repr__(self):
        return "Pattern(" + self.name + ", " + str(self.args) + ")"

def createPattern(s, loc, toks):
    return Pattern(toks[0][0], toks[0][1].asList(), loc)

class Constructor(object):
    def __init__(self, name, args, loc):
        self.name = name
        self.args = args
        self.loc = loc
    def __repr__(self):
        return "Constructor(" + self.name + ", " + str(self.args) + ")" 

def createConstructor(s, loc, toks):
    return Term(constructor=Constructor(toks[0][0], toks[0][1].asList(), loc))

class Call(object):
    def __init__(self, name, args, loc):
        self.name = name
        self.args = args
        self.loc = loc
    def __repr__(self):
        return "Call(" + self.name + ", " + str(self.args) + ")"

def createCall(s, loc, toks):
    return Term(call=Call(toks[0][0], toks[0][1].asList(), loc))

class FFun(object):
    def __init__(self, name, args, term):
        self.name = name
        self.args = args
        self.term = term
    def __repr__(self):
        return "FFun(" + self.name + ", " + str(self.args) + ", " + str(self.term) + ")"
    
def createFFun(s, loc, toks):
    return Fun(fFun=FFun(toks[0][0], toks[0][1].asList(), toks[0][2]))

class GFun(object):
    def __init__(self, name, pattern, args, term):
        self.name = name
        self.pattern = pattern
        self.args = args
        self.term = term
    def __repr__(self):
        return "GFun(" + self.name + ", " + str(self.pattern) + ", " + str(self.args) + ", " + str(self.term) + ")"

def createGFun(s, loc, toks):
    return Fun(gFun=GFun(toks[0][0], toks[0][1], toks[0][2].asList(), toks[0][3]))

lident = Word(string.lowercase, alphanums)
uident = Word(string.uppercase, alphanums)


LPAR = Suppress("(")
RPAR = Suppress(")")
EQ = Suppress("=")

term = Forward();

variable = Word(string.lowercase, alphanums).setParseAction(createPureVar)
tvariable = Word(string.lowercase, alphanums).setParseAction(createTermVar)

cargs = Group(Optional(LPAR + Optional(delimitedList(term)) + RPAR))
constructor = Group(uident + cargs).setParseAction(createConstructor)

patternName = uident
patternArgs = Group(Optional(LPAR + Optional(delimitedList(variable)) + RPAR))

pattern = Group(patternName + patternArgs).setParseAction(createPattern)

fArgs = Group(LPAR + Optional(delimitedList(variable)) + RPAR)
fFun = Group(lident + fArgs + EQ + term + Suppress(";")).setParseAction(createFFun)

gArgs = Group(Optional(Suppress(",") + delimitedList(variable)))
gFun = Group(lident + LPAR + pattern + gArgs + RPAR + EQ + term + Suppress(";")).setParseAction(createGFun)

callArgs = Group(LPAR + Optional(delimitedList(term)) + RPAR)
call = Group(lident + callArgs).setParseAction(createCall)
term << (call | tvariable | constructor)

fundef = gFun | fFun

program = OneOrMore(fundef)

UNDEF_VAR = "undefined variable %s at the right side of the definition"
WRONG_CONSTR = "%s is already defined as constructor with arity %s"
WRONG_FARGS = "Wrong number of arguments for function %s"
ALREADY_FFUN = "%s + is already defined as f-fun "
ALREADY_GFUN = "%s + is already defined as f-fun "
ALREADY_GFUN_AR = "%s is already defined as g-fun with arity %s"
SEC_VAR_OCC = "variable %s occurs the second time at the left side of the definition"
REP_GPATTERN = "%s with constructor % is already defined"

def parseAndValidate(text):
    funs = program.parseString(text, True).asList()
    validateProgram(funs, text)
    return funs

def validateProgram(prog, text):
    # arity of the first encountering of f-function in the left side
    farity = {}
    # arity of the first encountering of g-function in the left side
    garity = {}
    
    # populating farity and garity
    for f in prog:        
        if f.fFun:
            fFun = f.fFun
            fname = fFun.name
            if not farity.has_key(fname):
                arity = len(fFun.args)
                farity[fname] = arity
        else:
            gFun = f.gFun
            gname = gFun.name
            if not garity.has_key(gname):
                arity = len(gFun.args) + 1
                garity[gname] = arity    

    # info about arity of constructors
    cInfo = {}
    # info about arity of f-functions
    fInfo = {}
    # info about arity of g-functions
    gInfo = {}
    
    def validateDefinition(definition):
        fvars = set()
        
        def validateTerm(term):
            if term.variable:
                variable = term.variable
                if variable.name not in fvars:
                    raise ParseException(text, variable.loc, UNDEF_VAR % variable.name)
            elif term.constructor:
                constructor = term.constructor
                cname = constructor.name
                cargs = constructor.args
                arity = len(cargs)
                if cInfo.has_key(cname) and cInfo[cname] != arity:
                    raise ParseException(text, constructor.loc, WRONG_CONSTR % (cname , cInfo[cname]))
                else:
                    cInfo[cname] = arity
                    for arg in cargs: 
                        validateTerm(arg)
            else:
                call = term.call
                fname = call.name
                fargs = call.args
                arity = len(fargs)
                if farity.has_key(fname):
                    if farity[fname] != arity:
                        raise ParseException(text, call.loc, WRONG_FARGS % fname)
                elif garity.has_key(fname):
                    if garity[fname] != arity:
                        raise ParseException(text, call.loc, WRONG_FARGS % fname)
                else:
                    farity[fname] = arity
                for arg in fargs: validateTerm(arg)
        
        if definition.fFun:
            ffun = definition.fFun
            fname = ffun.name
            fargs = ffun.args
            fterm = ffun.term
            if fInfo.has_key(fname): raise ParseException(text, ffun.loc, ALREADY_FFUN % fname)
            if gInfo.has_key(fname): raise ParseException(text, ffun.loc, ALREADY_GFUN % fname)
            fInfo[fname] = len(fargs)
            for v in fargs:
                if v.name in fvars: raise ParseException(text, v.loc, SEC_VAR_OCC % v.name) 
                fvars.add(v.name)
            validateTerm(fterm)
        else:
            gfun = definition.gFun
            gname = gfun.name
            gargs = gfun.args
            gterm = gfun.term
            gpattern = gfun.pattern
            if fInfo.has_key(gname): raise ParseException(text, ffun.loc, ALREADY_FFUN % gname)
            cName = gpattern.name
            if cInfo.has_key(cName):
                cArity = cInfo[cName]
                if cArity != len(gpattern.args): ParseException(text, constructor.loc, WRONG_CONSTR % (cname , cArity))
            else:
                cInfo[cName] = len(gpattern.args)
            if gInfo.has_key(gname):
                (arity, cNames) = gInfo[gname]
                if arity != len(gargs) + 1: raise ParseException(text, ffun.loc, ALREADY_GFUN_AR % (gname, arity))
                if cName in cNames: raise ParseException(text, ffun.loc, REP_GPATTERN % (gname, cName))
                cNames.add(cName)
            else:
                gInfo[gname] = (len(gargs) + 1, set(cName))
            for v in gpattern.args:
                if v.name in fvars: raise ParseException(text, v.loc, SEC_VAR_OCC % v.name)
                fvars.add(v)
            for v in gpattern.args:
                if v.name in fvars: raise ParseException(text, v.loc, SEC_VAR_OCC % v.name)
                fvars.add(v.name)
            for v in gargs:
                if v.name in fvars: raise ParseException(text, v.loc, SEC_VAR_OCC % v.name)
                fvars.add(v.name)
            validateTerm(gterm)
    for f in prog: validateDefinition(f)