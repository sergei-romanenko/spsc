require 'SLL'
require 'Algebra'

module MSG
  # MSG = Most Specific Generalization
  class Gen
    def initialize(exp, substA, substB)
      @exp = exp
      @substA = substA
      @substB = substB
    end

    def to_s
      assocA_s = @substA.to_a.sort.map{|v, e| "#{v}=#{e}"} * ","
      assocB_s = @substB.to_a.sort.map{|v, e| "#{v}=#{e}"} * ","
      "#{@exp} =>> {#{assocA_s}}{#{assocB_s}}"
    end
  end

  class MSGBuilder
    include SLL
    include Algebra
    def initialize(nameGen)
      @nameGen = nameGen
      @exp = nil
      @subst = nil
      @noProgress = nil
    end

    def build(e1, e2)
      vname = @nameGen.freshName()
      @exp = Var.new(vname)
      @subst = {vname => [e1, e2]}
      while true
        @noProgress = true
        mergeSubexp()
        commonFunctor() if @noProgress
        break if @noProgress
      end
      subst1 = {}
      subst2 = {}
      @subst.each do |vname, e1e2|
        e1, e2 = e1e2
        subst1[vname] = e1
        subst2[vname] = e2
      end
      result = Gen.new(@exp, subst1, subst2)
      @exp = nil
      @subst = nil
      result
    end

    def commonFunctor()
      @subst.each_pair do |vname, e1e2|
        e1, e2 = e1e2
        if e1.hasTheSameFunctorAs?(e2)
          @noProgress = false
          ns = @nameGen.freshNameList(e1.args.length)
          vs = ns.map{|x| Var.new(x)}
          @exp = @exp.applySubst({vname => e1.cloneFunctor(vs)})
          @subst.delete(vname)
          x = ns.zip(e1.args.zip(e2.args))
          y = assoc_to_h(x)
          @subst = @subst.merge(assoc_to_h(ns.zip(e1.args.zip(e2.args))))
          return
        end
      end
    end

    def aMergeableKeyPair()
      @subst.each_key do |i|
        @subst.each_key do |j|
          return [i, j] if i < j && @subst[i] == @subst[j]
        end
      end
      return nil
    end

    def mergeSubexp()
      ij = aMergeableKeyPair()
      return if ij == nil
      i, j = ij
      @noProgress = false
      @exp = @exp.applySubst({i => Var.new(j)})
      @subst.delete(i)
    end
  end
end