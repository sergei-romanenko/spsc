module Algebra

using SPSC.SLanguage

theSameFunctor(e1::CFG, e2::CFG)::Bool =
  e1.kind == e2.kind &&
  e1.name == e2.name &&
  length(e1.args) == length(e2.args)

Base.:(==)(e1::Exp, e2::Exp) = false
Base.:(==)(e1::Var, e2::Var) = e1.name == e2.name
Base.:(==)(e1::CFG, e2::CFG) =
  theSameFunctor(e1, e2) && e1.args == e2.args

vars(e::Exp)::Params = []

vars(e::Var) = [e.name]

function vars(e::CFG)
  # We don't use sets here, in order to preserve
  # the original order of variables in the expression.
  # (The order is preserved just for readability of
  # residual programs.)
    vs::Params = []
    for arg in e.args
        for v in vars(arg)
            if !(v in vs)
                push!(vs, v)
            end
        end
    end
    return vs
end

Subst = Dict{Name,Exp}
UNS = Union{Nothing,Subst}

function substToString(s::Subst)
  kvs = ["$key->$(s[key]);" for key in sort(collect(keys(s)))]
  string(kvs...)
end

applySubst(s::Subst) =
  e::Exp -> applySubst(s, e)

applySubst(s::Subst, v::Var)::Exp = get(s, v.name, v)

applySubst(s::Subst, e::CFG)::Exp =
  CFG(e.kind, e.name, [applySubst(s, arg) for arg in e.args])

matchAgainstAcc!(s::Subst, e1::Exp, e2::Exp)::Bool = false  

function matchAgainstAcc!(s::Subst, v::Var, e2::Exp)
    e = get(s, v.name, nothing)
    if e isa Nothing
        s[v.name] = e2
        return true
    else
        return e == e2
    end
end

function matchAgainstAcc!(s::Subst, e1::CFG, e2::CFG)::Bool
  theSameFunctor(e1, e2) || return false
    matchAgainstAccL(s, e1.args, e2.args)
end

function matchAgainstAccL(s::Subst, args1::Args, args2::Args)::Bool
    for (e1, e2) in zip(args1, args2)
        matchAgainstAcc!(s, e1, e2) || return false
    end
    return true
end

function matchAgainst(e1::Exp, e2::Exp)::UNS
    s = Subst()
    matchAgainstAcc!(s, e1, e2) ? s : nothing
end

instOf(e1::Exp, e2::Exp)::Bool =
  !(matchAgainst(e2, e1) isa Nothing)

equiv(e1::Exp, e2::Exp)::Bool =
  instOf(e1, e2) && instOf(e2, e1)

# Name generator

mutable struct NameGen
  prefix::String
  tick::Int64
end

function freshName(ng::NameGen)
  tick = ng.tick
  ng.tick = tick+1
  string(ng.prefix, tick)
end

function freshNameList(ng::NameGen, n)
  tick = ng.tick
  ng.tick = tick + n
  [string(ng.prefix, tick+k) for k in 0:n-1]
end

export theSameFunctor, vars
export Subst, substToString, UNS, applySubst
export matchAgainst
export instOf, equiv
export NameGen, freshName, freshNameList

end
