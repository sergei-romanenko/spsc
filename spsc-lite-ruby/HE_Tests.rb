require 'test/unit'
require 'SLL'
require 'SLLParser'
require 'HE'

class HE_Tests < Test::Unit::TestCase

  include SLL
  include SLLParser
  include HE
  def heTrue(msg, input1, input2)
    e1 = pExp(input1)
    e2 = pExp(input2)
    assert(he(e1, e2), msg)
  end

  def heFalse(msg, input1, input2)
    e1 = pExp(input1)
    e2 = pExp(input2)
    assert(!he(e1, e2), msg)
  end

  def varAttackTrue(input)
    e = pExp(input)
    assert(aVarIsUnderAttack(e))
  end

  def varAttackFalse(input)
    e = pExp(input)
    assert(!aVarIsUnderAttack(e))
  end

  def test101VarAttack()
    varAttackTrue("x")
    varAttackFalse("A")
    varAttackFalse("f(x)")
    varAttackTrue("g(x,y)")
    varAttackTrue("g1(g2(x))")
    varAttackFalse("g(A)")
    varAttackFalse("g(f(x))")
  end

  def test201VV()
    heTrue("v1 <| v2", "v1", "v2")
  end

  def test202VF()
    heTrue("v1 <| F(v2)", "v1", "F(v2)")
  end

  def test203FV()
    heFalse("not F(v2) <| v1", "F(v2)", "v1")
  end

  def test204Diving()
    heTrue("F(v1) < G(v0, F(G(v2)))", "F(v1)", "G(v0,F(H(v2)))")
  end

  def test205Coupling1()
    heTrue("F(v1, G(v2)) <| F(H(w1), G(w2))", "F(v1,G(v2))", "F(H(w1),G(w2))")
  end

  def test206Coupling2()
    heFalse("not f(v1) <| g(w1)", "f(v1)", "g(w1)")
  end

end