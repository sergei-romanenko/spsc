require 'test/unit'
require 'SLL'
require 'SLLParser'
require 'Algebra'
require 'BasicProcessTreeBuilder'

class BasicProcessTreeBuilder_Tests < Test::Unit::TestCase
  include SLL
  include SLLParser
  include Algebra
  include ProcessTreeBuilder
  def setup()
    @pAdd = "gAdd(Z,y)=y;gAdd(S(x),y)=S(gAdd(x,y));"
    @pAddAcc = "gAddAcc(Z,y)=y;gAddAcc(S(x),y)=gAddAcc(x,S(y));"
  end

  def drStep(prog, e, expected)
    drStep0(pProg(prog), pExp(e), expected)
  end

  def drStep0(prog, e, expected)
    trBuilder = DrivingEngine.new(NameGen.new("v", 100), prog)
    branches = trBuilder.drivingStep(e)
    branches_s = branches.map{|b| "(#{b[0]},#{b[1]})"}.join("")
    assert_equal(expected, branches_s)
  end

  def buildPrTree1(prog, e)
    nameGen = NameGen.new("v", 100)
    BasicBuilder.build(nameGen, 1, prog, e)
  end

  def buildPrTree1OK(prog, e, expected)
    tree = buildPrTree1(pProg(prog), pExp(e))
    assert_equal(expected, tree.to_s)
  end

  def buildPrTree(prog, e)
    nameGen = NameGen.new("v", 100)
    BasicBuilder.build(nameGen, 100, prog, e)
  end

  def buildPrTreeOK(prog, e, expected)
    tree = buildPrTree(pProg(prog), pExp(e))
    assert_equal(expected, tree.to_s)
  end

  def test101Ctr
    drStep("", "C(a,b)", "(a,)(b,)")
  end

  def test102FCall()
    drStep("f(x)=x;", "f(A(z))", "(A(z),)")
  end

  def test103GCallCtr()
    drStep(@pAddAcc, "gAddAcc(S(S(Z)),Z)", "(gAddAcc(S(Z),S(Z)),)")
  end

  def test104GCallVar()
    drStep(@pAddAcc, "gAddAcc(a,b)", "(b,a=Z)(gAddAcc(v100,S(b)),a=S(v100))")
  end

  def test105GCallGeneral()
    drStep(@pAddAcc, "gAddAcc(gAddAcc(a,b),c)",
    "(gAddAcc(b,c),a=Z)(gAddAcc(gAddAcc(v100,S(b)),c),a=S(v100))")
  end

  def test106Let()
    drStep0(SLL::Program.new([]),
    Let.new(Ctr.new("C", [Var.new("x"), Var.new("y")]), [["x", Var.new("a")], ["y", Var.new("b")]]),
    "(C(x,y),)(a,)(b,)")
  end

  def test201PrTrVar1()
    buildPrTree1OK("", "x",
    "{0:(x,,,[])}")
  end

  def test201PrTrCtr1()
    buildPrTree1OK("", "S(Z)",
    "{0:(S(Z),,,[1]),1:(Z,,0,[])}")
  end

  def test202AddS_Z1()
    buildPrTree1OK(@pAddAcc, "gAddAcc(S(Z),Z)",
    "{0:(gAddAcc(S(Z),Z),,,[1]),1:(gAddAcc(Z,S(Z)),,0,[])}")
  end

  def test203AddAB1()
    buildPrTree1OK(@pAdd, "gAdd(a,b)",
    "{0:(gAdd(a,b),,,[1,2]),1:(b,a=Z,0,[]),2:(S(gAdd(v100,b)),a=S(v100),0,[])}")
  end

  def test204AddAdd1()
    buildPrTree1OK(@pAdd, "gAdd(gAdd(a,b),c)",
    "{0:(gAdd(gAdd(a,b),c),,,[1,2]),1:(gAdd(b,c),a=Z,0,[]),2:(gAdd(S(gAdd(v100,b)),c),a=S(v100),0,[])}")
  end

  def test205AddAccAB()
    buildPrTreeOK(@pAddAcc, "gAddAcc(a,b)",
    "{0:(gAddAcc(a,b),,,[1,2]),1:(b,a=Z,0,[]),2:(let a=v100,b=S(b) in gAddAcc(a,b),a=S(v100),0,[3,4,5]),3:(gAddAcc(a,b),,2,[]),4:(v100,,2,[]),5:(S(b),,2,[6]),6:(b,,5,[])}")
  end

  def test301AddS_Z()
    buildPrTreeOK(@pAddAcc, "gAddAcc(S(Z),Z)",
    "{0:(gAddAcc(S(Z),Z),,,[1]),1:(gAddAcc(Z,S(Z)),,0,[2]),2:(S(Z),,1,[3]),3:(Z,,2,[])}")
  end

  def test303AddAB()
    buildPrTreeOK(@pAdd, "gAdd(a,b)",
    "{0:(gAdd(a,b),,,[1,2]),1:(b,a=Z,0,[]),2:(S(gAdd(v100,b)),a=S(v100),0,[3]),3:(gAdd(v100,b),,2,[])}")
  end

  def test304AddAdd()
    buildPrTreeOK(@pAdd, "gAdd(gAdd(a,b),c)",
    "{0:(gAdd(gAdd(a,b),c),,,[1,2]),1:(gAdd(b,c),a=Z,0,[3,4]),3:(c,b=Z,1,[]),4:(S(gAdd(v101,c)),b=S(v101),1,[5]),5:(gAdd(v101,c),,4,[]),2:(gAdd(S(gAdd(v100,b)),c),a=S(v100),0,[6]),6:(S(gAdd(gAdd(v100,b),c)),,2,[7]),7:(gAdd(gAdd(v100,b),c),,6,[])}")
  end
end
