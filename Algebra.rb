require 'SLL'

module Algebra

  extend SLL
  class Matcher
    attr_reader :subst
    def initialize()
      @subst = {}
    end

    def match(pat, exp)
      if pat.isVar()
        e = @subst[pat.vname]
        if e == nil
          @subst[pat.vname] = exp
        elsif e != exp
          @subst = nil
        end
      elsif pat.isCall() && pat.hasTheSameFunctorAs?(exp)
        pat.args.length.times do |i|
          match(pat.args[i], exp.args[i])
          return if @subst == nil
        end
      else
        @subst = nil
      end
    end
  end

  def matchAgainst(pat, exp)
    matcher = Matcher.new()
    matcher.match(pat, exp)
    return matcher.subst
  end

  def instOf(e1, e2)
    matchAgainst(e2, e1) != nil
  end

  def equiv(e1, e2)
    instOf(e1, e2) && instOf(e2, e1)
  end

  class NameGen
    def initialize(prefix, seed)
      @prefix = prefix
      @tick = seed
    end

    def freshName()
      tick = @tick
      @tick = tick+1
      "#{@prefix}#{tick}"
    end

    def freshNameList(n)
      tick = @tick
      @tick = tick + n
      n.times.map {|k| "#{@prefix}#{tick+k}"}
    end
  end

end