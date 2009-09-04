require 'rubygems'
require 'test/unit'
require 'rparsec'
require 'rparsec/parsers'
require 'SLL'
require 'SLLParser'

class SLLParser_Tests < Test::Unit::TestCase
  include SLLParser
  def exc(p, input)
    assert_raises(ParserException){(p << Parsers::eof).parse(input)}
  end

  def stringOK(p, expected, input)
    assert_equal(expected, (p << Parsers::eof).parse(input))
  end

  def parseOK(p, expected, input)
    assert_equal(expected, (p << Parsers::eof).parse(input).to_s)
  end

  def expOK(expected, input)
    parseOK(Expression, expected, input)
  end

  def programOK(expected, input)
    parseOK(Program, expected, input)
  end

  def test101Idents()
    stringOK(LIdent, 'aZ12b', 'aZ12b')
    stringOK(UIdent, "Aaa9b", "Aaa9b")
    stringOK(FIdent, "fAa9b", "fAa9b")
    stringOK(GIdent, "gAa9b", "gAa9b")
    exc(LIdent, "Aa")
    exc(UIdent, "aa")
    exc(LIdent, "A*a")
  end

  def test102Call()
    expOK("x", "x")
    expOK("fA", "fA")
    expOK("gA", "gA")
    expOK("A(x,y)", "A(x,y)")
    expOK("A", "A()")
    expOK("A", "A")
    expOK("fA(x,y)", "fA(x,y)")
    expOK("fA()", "fA()")
    expOK("gA(x,y)", "gA(x,y)")
    exc(Expression, "gA()")
  end

  def test103Program()
    programOK("", "")
    programOK("f(x)=A;", "f(x)=A;")
    programOK("f(x)=A;f1(x)=A1;", "f(x)=A;f1(x)=A1;")
    programOK("f()=f();", "f()=f();")
    programOK("g(C)=A;", "g(C)=A;")
    programOK("g(C(x))=A;", "g(C(x))=A;")
    programOK("g(C(x))=A;", "g(C(x))=A;")
    programOK("g(C(x,y),z)=A;", "g(C(x,y),z)=A;")
    programOK("g(C,y)=A;", "g(C,y)=A;")
  end
end