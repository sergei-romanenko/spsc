module SLanguage

abstract type Exp end

const Name = String
const Arg = Exp
const Args = Vector{Arg}
const Params = Vector{Name}

@enum CKind Ctr FCall GCall

struct Binding
    name::Name
    exp::Exp
end

struct Var <: Exp
    name::Name
end
    
struct CFG <: Exp
    kind::CKind
    name::Name
    args::Args
end

isFGCall(e::Exp) = false
isFGCall(e::CFG) = e.kind == FCall || e.kind == GCall

struct Let <: Exp
    exp::Exp
    bindings::Vector{Binding}
end

abstract type Rule end

struct FRule <: Rule
    name::Name
    params::Params
    body::Exp
end

struct GRule <: Rule
    name::Name
    cname::Name
    cparams::Params
    params::Params
    body::Exp
end

struct Program
    rules::Vector{Rule}
end

export CKind, Name, Arg, Args, Params, Binding
export Exp, Var, CFG, Ctr, FCall, GCall, Let, Rule, FRule, GRule, Program
export isFGCall

end
