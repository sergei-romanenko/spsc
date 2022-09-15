module MSG

using SPSC.SLanguage
using SPSC.SParsers
using SPSC.ShowUtil
using SPSC.Algebra

struct Gen
    e::Exp
    s1::Subst
    s2::Subst
end

function Base.show(io::IO, gen::Gen)
    print(io, "")
    print(io, gen.e)
    print(io, " =>> {")
    print(io, substToString(gen.s1))
    print(io, "}{")
    print(io, substToString(gen.s2))
    print(io, "}")
end

function commonFunctor!(ng::NameGen, e::Exp, s1::Subst, s2::Subst)::Exp
    for n in keys(s1)
        e1 = s1[n]
        e2 = s2[n]
        (e1 isa CFG && e2 isa CFG && theSameFunctor(e1, e2)) || continue
        c1 = e1::CFG
        c2 = e2::CFG
        l = length(c1.args)
        ns = freshNameList(ng, l)
        delete!(s1, n)
        merge!(s1, Subst(zip(ns, c1.args)))
        delete!(s2, n)
        merge!(s2, Subst(zip(ns, c2.args)))
        return applySubst(Subst(n => CFG(c1.kind, c1.name, [Var(ns[i]) for i in 1:l])), e)
    end
    return e
end

function commonSubst!(e::Exp, s1::Subst, s2::Subst)::Exp
    for (n1, e1) in s1, (n2, e2) in s1
        if (n1 != n2 && e1 == e2) && (s2[n1] == s2[n2])
            delete!(s1, n1)
            delete!(s2, n1)
            return applySubst(Subst(n1 => Var(n2)), e)
        end
    end
    return e
end

function msg(ng::NameGen, e1::Exp, e2::Exp)::Gen
    n = freshName(ng)
    s1 = Subst(n => e1)
    s2 = Subst(n => e2)
    e = Var(n)
    while true
        old_e = e
        e = commonSubst!(commonFunctor!(ng, e, s1, s2), s1, s2)
        (e == old_e) && break
    end
    return Gen(e, s1, s2)
end

export Gen, msg

end
