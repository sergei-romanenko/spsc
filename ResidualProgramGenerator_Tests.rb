require 'test/unit'
require 'SLL'
require 'SLLParser'
require 'Algebra'
require 'BasicProcessTreeBuilder'
require 'AdvancedProcessTreeBuilder'
require 'ResidualProgramGenerator'

class ResidualProgramGenerator_Tests < Test::Unit::TestCase
  include SLLParser
  include Algebra
  include ProcessTreeBuilder
  def setup
    # Sample programs
    @pAdd = "gAdd(Z,y)=y;gAdd(S(x),y)=S(gAdd(x,y));"
    @pAddAcc = "gAddAcc(Z,y)=y;gAddAcc(S(x),y)=gAddAcc(x,S(y));"
  end

  #---- Basic supercompiler

  def runBasicScp(prog, e)
    nameGen = NameGen.new("v", 100)
    tree = BasicBuilder.build(nameGen, 100, prog, e)
    res = ResidualProgramGenerator.new(tree).genResidualProgram()
    return res
  end

  def basicScpOK(prog, e, expected)
    (resPr, resExp) = runBasicScp(pProg(prog), pExp(e))
    res_s = "#{resPr}$#{resExp}"
    assert_equal(expected, res_s)
  end

  def test101BVar()
    basicScpOK("", "a", "$a")
  end

  def test102BCtr()
    basicScpOK("", "C(a,b)", "$C(a,b)")
  end

  def test103BAddAB()
    basicScpOK(@pAdd, "gAdd(a,b)",
    "gAdd1(Z,b)=b;gAdd1(S(v100),b)=S(gAdd1(v100,b));$gAdd1(a,b)")
  end

  def test104BAddAdd()
    basicScpOK(@pAdd, "gAdd(gAdd(a,b),c)",
    "gAdd2(Z,c)=c;gAdd2(S(v101),c)=S(gAdd2(v101,c));gAdd1(Z,b,c)=gAdd2(b,c);gAdd1(S(v100),b,c)=S(gAdd1(v100,b,c));$gAdd1(a,b,c)")
  end

  def test105BAddAccAB()
    basicScpOK(@pAddAcc, "gAddAcc(a,b)",
    "gAddAcc1(Z,b)=b;gAddAcc1(S(v100),b)=gAddAcc1(v100,S(b));$gAddAcc1(a,b)")
  end

  #---- Advanced supercompiler

  def runAdvancedScp(prog, e)
    nameGen = NameGen.new("v", 100)
    tree = AdvancedBuilder.build(nameGen, 100, prog, e)
    res = ResidualProgramGenerator.new(tree).genResidualProgram()
    res
  end

  def advancedScpOK(prog, e, expected)
    resPr, resExp = runAdvancedScp(pProg(prog), pExp(e))
    res_s = "#{resPr}$#{resExp}"
    assert_equal(expected, res_s)
  end

  def test201AdvAddAB()
    advancedScpOK(@pAdd, "gAdd(a,b)",
    "gAdd1(Z,b)=b;gAdd1(S(v100),b)=S(gAdd1(v100,b));$gAdd1(a,b)")
  end

  def test202AdvAddAA()
    advancedScpOK(@pAdd, "gAdd(a,a)",
    "gAdd1(Z,v103)=v103;gAdd1(S(v104),v103)=S(gAdd1(v104,v103));$gAdd1(a,a)")
  end

  def test203AdvAddAdd()
    advancedScpOK(@pAdd, "gAdd(gAdd(a,b),c)",
    "gAdd2(Z,c)=c;gAdd2(S(v101),c)=S(gAdd2(v101,c));gAdd1(Z,b,c)=gAdd2(b,c);gAdd1(S(v100),b,c)=S(gAdd1(v100,b,c));$gAdd1(a,b,c)")
  end

  def test204AdvAddAccAB()
    advancedScpOK(@pAddAcc, "gAddAcc(a,b)",
    "gAddAcc1(Z,b)=b;gAddAcc1(S(v100),b)=gAddAcc1(v100,S(b));$gAddAcc1(a,b)")
  end

  def test205AdvAddAccAA()
    advancedScpOK(@pAddAcc, "gAddAcc(a,a)",
    "gAddAcc1(Z,v103)=v103;gAddAcc1(S(v104),v103)=gAddAcc1(v104,S(v103));$gAddAcc1(a,a)")
  end

  def test206AdvAddAccAddAcc()
    advancedScpOK(@pAddAcc, "gAddAcc(gAddAcc(a,b),c)",
    "gAddAcc2(Z,c)=c;gAddAcc2(S(v101),c)=gAddAcc2(v101,S(c));gAddAcc1(Z,b,c)=gAddAcc2(b,c);gAddAcc1(S(v100),b,c)=gAddAcc1(v100,S(b),c);$gAddAcc1(a,b,c)")
  end

end