require 'BasicProcessTreeBuilder'
require 'HE'
require 'MSG'

module ProcessTreeBuilder
  class AdvancedBuilder < BasicBuilder
    include SLL
    include ProcessTree
    include HE
    include MSG
    def initialize(drivingEngine, exp)
      @drivingEngine = drivingEngine
      @tree = Tree.new(exp)
      @nameGen = drivingEngine.nameGen
      @msgBuilder = MSGBuilder.new(@nameGen)
    end

    def abstract(alpha, exp, subst)
      bindings = subst.to_a.sort
      letExp = Let.new(exp, bindings)
      @tree.replaceSubtree(alpha, letExp)
    end

    def split(beta)
      exp = beta.exp
      args = exp.args
      names1 = @nameGen.freshNameList(args.length)
      args1 = names1.map{|x| Var.new(x)}
      letExp = Let(beta.e.cloneFunctor(args1), names1.zip(args))
      @tree.replaceSubtree(beta, letExp)
    end

    def generalizeAlphaOrSplit(beta, alpha)
      gen = @msgBuilder.build(alpha.exp, beta.exp)
      if gen.exp.isVar()
        split(beta)
      else
        abstract(alpha, gen.exp, gen.substA)
      end
    end

    def findEmbeddedAncestor(beta)
      beta.ancestors.find do |alpha|
        alpha.exp.isFGCall() && embeddedIn(alpha.exp, beta.exp)
      end
    end

    def buildStep(beta)
      # This method overrides the one in the basic version of
      # the process tree builder.
      alpha = beta.findMoreGeneralAncestor()
      if alpha
        loopBack(beta, alpha)
      else
        alpha = findEmbeddedAncestor(beta)
        if alpha
          generalizeAlphaOrSplit(beta, alpha)
        else
          expandNode(beta)
        end
      end
    end

    def AdvancedBuilder.build(nameGen, k, prog, exp)
      drivingEngine = DrivingEngine.new(nameGen, prog)
      builder = AdvancedBuilder.new(drivingEngine, exp)
      builder.buildProcessTree(k)
      builder.tree
    end
  end
end