require 'SLL'
require 'Algebra'
require 'ProcessTree'

module BasicProcessTreeBuilder
  class DrivingEngine
    include SLL
    include Algebra
    include ProcessTree
    def initialize(nameGen, prog)
      # The program is supposed to be correct: no duplicate definitions, etc.
      @nameGen = nameGen
      @fRule = Hash.new
      @gRules = Hash.new
      @gcRule = Hash.new
      prog.rules.each do |rule|
        name = rule.name
        if rule.is_a?(FRule)
          @fRule[name] = rule
        elsif rule.is_a?(GRule)
          if @gRules.has_key?(name)
            #  Lists are mutable!
            @gRules[name] << rule
          else
            @gRules[name] = [rule]
          end
          @gcRule[[rule.name, rule.cname]] = rule
        else
          raise "Invalid rule"
        end
      end
    end

    def drivingStep(e)
      if e.isCtr()
        e.args.map{|arg| [arg, nil]}
      elsif e.isFCall()
        rule = @fRule[e.name]
        p2a = assoc_to_h(rule.params.zip(e.args))
        body = rule.body.applySubst(p2a)
        [[body, nil]]
      elsif e.isGCall()
        arg0 = e.args[0]
        args = e.args[1..-1]
        if arg0.isCtr()
          cname = arg0.name
          cargs = arg0.args
          rule = @gcRule[[e.name, cname]]
          p2a1 = assoc_to_h(rule.cparams.zip(cargs))
          p2a2 = assoc_to_h(rule.params.zip(args))
          body = rule.body.applySubst(p2a1.merge(p2a2))
          [[body, nil]]
        elsif arg0.isVar()
          @gRules[e.name].map{|rule| driveBranch(e, rule)}
        else
          drivingStep(arg0).map{|b| [GCall.new(e.name, [b[0]] + args), b[1]]}
        end
      elsif e.isLet()
        [[e.body, nil]] + e.bindings.map{|ve|[ve[1], nil]}
      else
        raise "Unknown expression type"
      end
    end

    def driveBranch(e, rule)
      vname = e.args[0].vname
      cname = rule.cname
      cparams = @nameGen.freshNameList(rule.cparams.length)
      params = rule.params
      #              cargs = [Var(vn) for vn in cparams]
      cargs = cparams.map{|vn| Var.new(vn)}
      vname2ctr = {vname => Ctr.new(cname, cargs)}
      e1 = e.applySubst(vname2ctr)
      branches = drivingStep(e1)
      e2 = branches[0][0]
      [e2, Contraction.new(vname, cname, cparams)]
    end
  end

  class BasicProcessTreeBuilder
    include Algebra
    include ProcessTree
    attr_reader :tree
    def initialize(drivingEngine, exp)
      @drivingEngine = drivingEngine
      @tree = Tree.new(exp)
    end

    # The parts common to the basic and advanced supercompilers.

    # If beta `instOf` alpha, we generalize beta by introducing
    # a let-expression, in order to make beta the same as alpha
    # (modulo variable names).

    def loopBack(beta, alpha)
      subst = matchAgainst(alpha.exp, beta.exp)
      bindings = subst.to_a.sort
      letExp = SLL::Let.new(alpha.exp, bindings)
      @tree.replaceSubtree(beta, letExp)
    end

    # This function applies a driving step to the node's expression,
    # and, in general, adds children to the node.

    def expandNode(beta)
      branches = @drivingEngine.drivingStep(beta.exp)
      @tree.addChildren(beta, branches)
    end

    # Basic supercompiler process tree builder

    def buildStep(beta)
      #  This method is overridden in the advanced version of
      #  the process tree builder.
      alpha = beta.findMoreGeneralAncestor()
      if alpha
        loopBack(beta, alpha)
      else
        self.expandNode(beta)
      end
    end

    def buildProcessTree(k)
      # Specifying k = -1 results in an unlimited building loop.
      loop do
        break if k == 0
        k -= 1
        beta = @tree.findUnprocessedNode()
        break if not beta
        buildStep(beta)
      end
    end
  end

  def BasicProcessTreeBuilder.build(nameGen, k, prog, exp)
    drivingEngine = DrivingEngine.new(nameGen, prog)
    builder = BasicProcessTreeBuilder.new(drivingEngine, exp)
    builder.buildProcessTree(k)
    builder.tree
  end

end