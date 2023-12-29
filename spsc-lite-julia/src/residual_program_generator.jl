module ResidualProgramGenerator

using SPSC.SLanguage
using SPSC.Algebra
using SPSC.ProcessTree

const Sig = Tuple{Name,Vector{Name}}
const Sigs = Dict{NodeId,Sig}

struct ResPrGen
    tree::Tree
    sigs::Sigs
    rules::Vector{Rule}
end

ResPrGen(tree::Tree) = ResPrGen(tree, Sigs(), Rule[])

function genResidualProgram(tree::Tree)
    g = ResPrGen(tree)
    resExp = genExp!(g, tree.root)
    return (Program(g.rules), resExp)
end

function genExp!(g::ResPrGen, beta::Node)::Exp
    genExp!(g, beta, funcAncestor(beta))
end

function genExp!(g::ResPrGen, beta::Node, alpha::Node)::Exp
    (name, params) = g.sigs[alpha.nodeId]
    args = [Var(param) for param in params]
    subst = matchAgainst(alpha.e, beta.e)
    contr = alpha.children[1].contr
    if contr === nothing
        return applySubst(subst, CFG(FCall, name, args))
    else
        return applySubst(subst, CFG(GCall, name, args))
    end
end

function genExp!(g::ResPrGen, beta::Node, ::Nothing)::Exp
    genExp!Beta(g, beta, beta.e)
end

function genExp!Beta(g::ResPrGen, beta::Node, e::Var)::Exp
    return e
end

function genExp!Beta(g::ResPrGen, beta::Node, e::CFG)::Exp
    if isCtr(e)
        resExps = Exp[genExp!(g, n) for n in beta.children]
        return CFG(Ctr, e.name, resExps)
    else
        return genCall!(g, beta)
    end
end

function genExp!Beta(g::ResPrGen, beta::Node, e::Let)::Exp
    resExpList = Exp[genExp!(g, n) for n in beta.children]
    vnames = [b.name for b in e.bindings]
    subst = Dict(zip(vnames, resExpList[2:end]))
    return applySubst(subst, resExpList[1])
end

function isVarTest(beta::Node)::Bool
    beta.children[1].contr !== nothing
end

function getFGSig!(g, prefix, beta, name, vs)
    sig = get(g.sigs, beta.nodeId, nothing)
    if sig !== nothing
        return sig
    else
        name1 = "$(prefix)$(name[2:end])$(length(g.sigs) + 1)"
        sig1 = (name1, vs)
        g.sigs[beta.nodeId] = sig1
        return sig1
    end
end

function genCall!(g::ResPrGen, beta::Node)
    e = beta.e
    name = e.name
    args = e.args
    params = vars(e)
    if isVarTest(beta)
        (name1, vs1) = getFGSig!(g, "g", beta, name, params)
        bodies = [genExp!(g, n) for n in beta.children]
        contrs = [(n.contr.cname, n.contr.cparams)
                  for n in beta.children]
        grules = [GRule(name1, cname1, cparams1, params[2:end], body1)
                  for ((cname1, cparams1), body1) in zip(contrs, bodies)]
        append!(g.rules, grules)
        return CFG(GCall, name1, [Var(param) for param in params])
    elseif isFuncNode(g.tree, beta)
        (name1, params1) = getFGSig!(g, "f", beta, name, params)
        body1 = genExp!(g, beta.children[1])
        push!(g.rules, FRule(name1, params1, body1))
        return CFG(FCall, name1, [Var(param) for param in params])
    else
        return genExp!(g, beta.children[1])
    end
end

export genResidualProgram

end
