module HE

using SPSC.SLanguage
using SPSC.Algebra

aVarIsUnderAttack(e::Exp)::Bool = false
aVarIsUnderAttack(v::Var) = true

function aVarIsUnderAttack(e::CFG)
    e.ckind isa GCall || return false
    aVarIsUnderAttack(e.args[1])
end

# This is the "classic" homeomorphic imbedding relation.

heByDiving(e1::Exp, e2::Exp)::Bool = false
heByDiving(e1::Exp, e2::CFG)::Bool =
  any(e -> he(e1, e), e2.args)

heByCoupling(e1::Exp, e2::Exp)::Bool = false
heByCoupling(e1::Var, e2::Var)::Bool = true

function heByCoupling(e1::CFG, e2::CFG)::Bool
    theSameFunctor(e1, e2) || return false
    all([he(arg1, arg2) for (arg1, arg2) in zip(e1.args, e2.args)])
end

he(e1, e2) = heByDiving(e1, e2) || heByCoupling(e1, e2)

# Enhanced homeomorphic embedding:
# expressions are compared only if they belong
# to the same category (as defined by `aVarIsUnderAttack`).

embeddedIn(e1::Exp, e2::Exp)::Bool =
  aVarIsUnderAttack(e1) == aVarIsUnderAttack(e2) && he(e1, e2)

export he, embeddedIn

end
