require 'test/unit'
require 'SLL'
require 'Algebra'
require 'SLLParser'

class Algebra_Tests < Test::Unit::TestCase
  include SLL
  include SLLParser
  include Algebra
  def to_s_test(expected, e)
    assert_equal(expected, e.to_s)
  end

  def test001_to_s()
    to_s_test('x', Var.new("x"))
    to_s_test('A(x,y)', Ctr.new("A", [Var.new("x"), Var.new("y")]))
    to_s_test('fX(x,y)', FCall.new("fX", [Var.new("x"), Var.new("y")]))
    to_s_test('gX(x,y)', GCall.new("gX", [Var.new("x"), Var.new("y")]))
    to_s_test('let x=y in y', Let.new(Var.new("y"), [["x", Var.new("y")]]))
  end

  def matchOK(pat, exp, expected)
    subst = matchAgainst(pExp(pat), pExp(exp))
    if subst != nil
      subst = subst.sort.map { |kv| "#{kv[0]}->#{kv[1]};"} * ''
    end
    assert_equal(expected, subst)
  end

  def matchNo(pat, exp)
    subst = matchAgainst(pExp(pat), pExp(exp))
    assert_equal(nil, subst)
  end

  def test101MatchV_E()
    matchOK("x", "S(Z)", "x->S(Z);")
  end

  def test102MatchC_V()
    matchNo("Z", "x")
  end

  def test103MatchC_C()
    matchOK("C(x,y)", "C(A,B)", "x->A;y->B;")
  end

  def test104MatchC1_C2()
      matchNo("C(x,y)", "D(A,B)")
  end
  
  def test105MatchC_F()
      matchNo("C(x,y)", "f(A,B)")
  end
  
  def test106MatchX_X_Eq()
      matchOK("C(x,x)", "C(A,A)", "x->A;")
  end
  
  def test107Match_X_XY()
      matchNo("C(x,y)", "C(A,B,C)")
  end
  
  def testMatch_XY_X()
      matchNo("C(x,y,z)", "C(A,B)")
  end

  def equivYes(e1, e2)
      assert(equiv(pExp(e1), pExp(e2)))
  end
  
  def test201EquivYes()
      equivYes("gA(fB(x,y),C)", "gA(fB(a,b),C)")
  end
  
  def equivNo(e1, e2)
      assert(!equiv(pExp(e1), pExp(e2)))
  end
  
  def test301EquivNo()
      equivNo("gA(fB(x,y),x)", "gA(fB(a,a),b)")
  end

end