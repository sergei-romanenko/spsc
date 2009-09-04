module SLL
  class Exp
    def isVar()   false end

    def isCall()  false end

    def isCtr()   false end

    def isFCall() false end

    def isGCall() false end

    def isFGCall()
      isFCall() or isGCall()
    end

    def isLet() false end

    def hasTheSameFunctorAs?(other)
      false
    end
  end

  class Var < Exp
    attr_reader :vname
    def initialize(vname)
      @vname = vname
    end

    def to_s
      @vname
    end

    def ==(other)
      other.is_a?(Exp) && other.isVar() && @vname == other.vname
    end

    def isVar() true end
  end

  class Call < Exp
    attr_reader :name, :args
    def initialize(name, args)
      @name = name
      @args = args
    end

    def to_s
      args_s = @args.each {|arg| arg.to_s}.join(',')
      "#{@name}(#{args_s})"
    end

    def ==(other)
      other.is_a?(Exp) && other.isCall() && @name == other.name && @args == other.args
    end

    def isCall() true end

    def hasTheSameFunctorAs?(e)
      e.is_a?(Call) && self.class == e.class && @name == e.name
    end
  end

  class Ctr < Call
    def initialize(name, args)
      super(name, args)
    end

    def to_s
      if @args.length == 0
        @name
      else
        super.to_s
      end
    end

    def isCtr() true  end
  end

  class FCall < Call
    def initialize(name, args)
      super(name, args)
    end

    def isFCall() true  end
  end

  class GCall < Call
    def initialize(name, args)
      super(name, args)
    end

    def isGCall() true  end
  end

  class Let < Exp
    attr_reader :exp, :bindings
    def initialize(exp, bindings)
      @exp = exp
      @bindings = bindings
    end

    def to_s
      bindings_s = @bindings.map {|binding| "#{binding[0]}=#{binding[1]}"}.join(',')
      "let #{bindings_s} in #{@exp.to_s}"
    end
    def isLet() true end
  end

  class FRule
    attr_reader :name, :params, :body
    def initialize(name, params, body)
      @name = name
      @params = params
      @body = body
    end

    def to_s
      "#{@name}(#{@params.join(',')})=#{@body.to_s};"
    end
  end

  class GRule
    attr_reader :name, :cname, :cparams, :params, :body
    def initialize(name, cname, cparams, params, body)
      @name = name
      @cname = cname
      @cparams = cparams
      @params = params
      @body = body
    end

    def to_s
      pat_s = "#{@cname}"
      pat_s << "(#{@cparams.join(',')})" if @cparams.length > 0
      pat_s << "," if @params.length > 0
      "#{@name}(#{pat_s}#{params.to_s})=#{body.to_s};"
    end
  end

  class Program
    def initialize(rules)
      @rules = rules
    end

    def to_s
      @rules.map{|rule| rule.to_s}.join("")
    end
  end

end
