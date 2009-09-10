require 'SLL'
require 'Algebra'
require 'ProcessTree'

class ResidualProgramGenerator
  include SLL
  include Algebra
  def initialize(tree)
    @tree = tree
    @sigs = Hash.new
    @rules = []
  end

  def genResidualProgram()
    resExp = genExp(@tree.root)
    [Program.new(@rules), resExp]
  end

  def genExp(beta)
    alpha = beta.funcAncestor()
    exp = beta.exp
    if not alpha
      if exp.isVar():
        exp
      elsif exp.isCtr()
        resExpList = genExpList(beta.children)
        Ctr.new(exp.name, resExpList)
      elsif exp.isFGCall()
        genCall(beta)
      elsif exp.isLet()
        resExpList = genExpList(beta.children)
        vnames = exp.bindings.map{|b| b[0]}
        x = vnames.zip(resExpList[1..-1])
        subst = assoc_to_h(vnames.zip(resExpList[1..-1]))
        resExpList[0].applySubst(subst)
      else
        raise "Invalid expression"
      end
    else
      name, params = @sigs[alpha]
      args = params.map{|param| Var.new(param)}
      subst = matchAgainst(alpha.exp, beta.exp)
      contr = alpha.children[0].contr
      if not contr
        return FCall.new(name, args).applySubst(subst)
      else
        return GCall.new(name, args).applySubst(subst)
      end
    end
  end

  def genExpList(nodes)
    nodes.map {|node| genExp(node) }
  end

  def isVarTest(beta)
    beta.children[0].contr ? true : false
  end

  def getFGSig(prefix, beta, name, vs)
    sig = @sigs[beta]
    if sig
      sig
    else
      name1 = "#{prefix}#{name[1..-1]}#{@sigs.length + 1}"
      sig1 = [name1, vs]
      @sigs[beta] = sig1
      sig1
    end
  end

  def getChContr(children)
    children.map{|n| [n.contr.cname, n.contr.cparams]}
  end

  def genCall(beta)
    exp = beta.exp
    name = exp.name
    args = exp.args
    params = exp.vars()
    if isVarTest(beta)
      name1, vs1 = getFGSig("g", beta, name, params)
      bodies = genExpList(beta.children)
      contrs = getChContr(beta.children)
      grules = contrs.zip(bodies).map do |contr1, body1|
        cname1, cparams1 = contr1
        GRule.new(name1, cname1, cparams1, params[1..-1], body1)
      end
      @rules += grules
      GCall.new(name1, params.map{|param| Var.new(param)})
    elsif @tree.isFuncNode(beta)
      name1, vs1 = getFGSig("f", beta, name, params)
      body1 = genExp(beta.children[0])
      @rules += FRule.new(name1, params1, body1)
      FCall.new(name1, params.map{|param| Var.new(param)})
    else
      genExp(beta.children[0])
    end
  end
end
