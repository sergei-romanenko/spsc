require 'test/unit'

#require 'SLL'
require 'SLLParser'
require 'Algebra'
require 'MSG'

class MSG_Tests < Test::Unit::TestCase
  include SLLParser
  include Algebra
  include MSG
  def msgOK(e1, e2, expected)
    msgBuilder = MSGBuilder.new(NameGen.new("v", 100))
    gen = msgBuilder.build(pExp(e1), pExp(e2))
    assert_equal(expected, gen.to_s)
  end

  def test101CommonFunctor()
    msgOK(
    "A(a1,C(a2,a3))",
    "A(b1,C(b2,b3))",
    "A(v101,C(v103,v104)) =>> {v101=a1,v103=a2,v104=a3}{v101=b1,v103=b2,v104=b3}")
  end

  def test102MergeSubexp1()
    msgOK(
    "f(a1,a2,a1)",
    "f(b1,b2,b1)",
    "f(v103,v102,v103) =>> {v102=a2,v103=a1}{v102=b2,v103=b1}")
  end

  def test103MergeSubexp2()
    msgOK(
    "f(a,a)",
    "f(b,S(b))",
    "f(v101,v102) =>> {v101=a,v102=a}{v101=b,v102=S(b)}")
  end

end