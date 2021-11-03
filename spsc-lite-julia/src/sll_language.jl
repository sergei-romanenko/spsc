module SLanguage

abstract type Exp end

const Name = String
const Arg = Exp
const Args = Vector{Arg}
const Params = Vector{Name}

#= @enum CKind begin
    Ctr
    FCall
    GCall
end =#

abstract type CKind end

struct Ctr <: CKind end
struct FCall <: CKind end
struct GCall <: CKind end

struct Binding
    name::Name
  exp::Exp
end

struct Var <: Exp
  name::Name
end
    
struct CFG <: Exp
  ckind::CKind
    name::Name
  args::Args
end

mkCtr(name::Name, args::Args) = CFG(Ctr(), name, args)
mkFCall(name::Name, args::Args) = CFG(FCall(), name, args)
mkGCall(name::Name, args::Args) = CFG(GCall(), name, args)

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
export mkCtr, mkFCall, mkGCall

end
