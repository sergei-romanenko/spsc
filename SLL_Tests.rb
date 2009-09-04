require 'test/unit'
require 'SLL'

class SLL_Tests < Test::Unit::TestCase

  include SLL
  def test101StrCall()
    assert_equal("C(x,y)", Ctr.new("C", [Var.new("x"), Var.new("y")]).to_s)
    assert_equal("C", Ctr.new("C", []).to_s)
    assert_equal("f(x,y)", FCall.new("f", [Var.new("x"), Var.new("y")]).to_s)
    assert_equal("g(x,y)", GCall.new("g", [Var.new("x"), Var.new("y")]).to_s)
  end

  def test102StrLet()
    assert_equal("let x=a,y=b in x", Let.new(Var.new("x"),
    [["x", Var.new("a")], ["y", Var.new("b")]]).to_s)
    assert_equal("let x=a in x", Let.new(Var.new("x"), [["x", Var.new("a")]]).to_s)
  end

  def test103Rule()
    assert_equal("f(x,y)=y;",  FRule.new("f", ["x", "y"], Var.new("y")).to_s)
    assert_equal("g(C(x),y)=y;", GRule.new("g", "C", ["x"], ["y"], Var.new("y")).to_s)
    assert_equal("g(C,y)=y;",  GRule.new("g", "C", [], ["y"], Var.new("y")).to_s)
    assert_equal("g(C)=C;",  GRule.new("g", "C", [], [], Ctr.new("C", [])).to_s)
  end

  def test104Program()
    assert_equal("f()=A;f1()=A1;",
    Program.new([FRule.new("f", [], Ctr.new("A",[])),
      FRule.new("f1", [], Ctr.new("A1",[]))]).to_s)
    assert_equal("g(C)=A;g1(C,x)=A;g2(C(x))=A;",
    Program.new([GRule.new("g", "C", [], [], Ctr.new("A",[])),
      GRule.new("g1", "C", [], ["x"], Ctr.new("A",[])),
      GRule.new("g2", "C", ["x"], [], Ctr.new("A",[]))]).to_s)
  end

  def test201TheSameFunctor()
    assert(Ctr.new("A", []).hasTheSameFunctorAs?(Ctr.new("A", [])))
    assert(FCall.new("A", []).hasTheSameFunctorAs?(FCall.new("A", [])))
    assert(GCall.new("A", []).hasTheSameFunctorAs?(GCall.new("A", [])))
    assert(! Ctr.new("A", []).hasTheSameFunctorAs?(Ctr.new("B", [])))
    assert(! Ctr.new("A", []).hasTheSameFunctorAs?(FCall.new("A", [])))
  end

  def test301Eq()
    assert(Var.new("x") ==  Var.new("x"), "x == x")
    assert(Var.new("x") !=  Var.new("y"), "x != y")
    assert(Ctr.new("A", []) == Ctr.new("A", []), "A == A")
    assert(Ctr.new("A", []) != Ctr.new("B", []), "A != B")
    assert([] ==  [], "[] == []")
    assert([Var.new("x")] ==  [Var.new("x")], "[x] == [x]")
    assert([Var.new("x")] != [Var.new("y")], "[x] == [y]")
    assert([Var.new("x")] != [Var.new("x"), Var.new("z")], "[x] != [x, z]")
    assert(Ctr.new("A", [Var.new("x")]) == Ctr.new("A", [Var.new("x")]), "A(x) == A(x)")
    assert(Ctr.new("A", [Var.new("x")]) != Ctr.new("A", [Var.new("y")]), "A(x) != A(y)")
  end

end