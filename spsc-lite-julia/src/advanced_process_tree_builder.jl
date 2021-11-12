module AdvancedProcessTreeBuilder

using SPSC.SLanguage
using SPSC.Algebra
using SPSC.ProcessTree
using SPSC.BasicProcessTreeBuilder
using SPSC.HE
using SPSC.MSG

### Advanced Supercompiler with homeomorphic imbedding and generalization  

struct AdvancedSC <: AbstractSC end

function abstract(tree::Tree, alpha::Node, e::Exp, subst::Subst)
    ks = collect(keys(subst))
    sort!(ks)
    bindings = [Binding(k, subst[k]) for k in ks]
    letExp = Let(e, bindings)
    replaceSubtree(tree, alpha, letExp)
end

function split(ng::NameGen, tree::Tree, beta::Node, e::CFG)
    names1 = freshNameList(ng, len(e.args))
    args1 = Exp[Var(x) for x in names1]
    e1 = CFG(e.kind, e.name, args1)
    bs1 = [Binding(b.name, b.e) for b in zip(names1, args)]
    letExp = Let(e1, bs1)
    replaceSubtree(tree, beta, letExp)
end

split(ng::NameGen, tree::Tree, beta::Node) = split(ng, tree, beta, beta.e)

function generalizeAlphaOrSplit(ng::NameGen, tree::Tree, beta::Node, alpha::Node)
    g = msg(ng, alpha.e, beta.e)
    if g.e isa Var
        split(ng, tree, beta)
    else
        abstract(tree, alpha, g.e, g.s1)
    end

end

function findEmbeddedAncestor(tree::Tree, beta::Node)
    for alpha in ancestors(beta)
        isFGCall(alpha.e) && embeddedIn(alpha.e, beta.e) && return alpha
    end
    return nothing
end

function BasicProcessTreeBuilder.buildStep(::AdvancedSC, d::DrivingEngine, tree::Tree, beta::Node)
    # This method overrides the method in the basic version of
    # the process tree builder.
    alpha = findMoreGeneralAncestor(beta)
    if alpha !== nothing
        loopBack(tree, beta, alpha)
    else
        alpha = findEmbeddedAncestor(tree, beta)
        if alpha !== nothing
            generalizeAlphaOrSplit(d.nameGen, tree, beta, alpha)
        else
            expandNode(d, tree, beta)
        end
    end
end

function buildAdvancedProcessTree(ng::NameGen, k::Int64, prog::Program, e::Exp)::Tree
    buildProcessTree(AdvancedSC(), ng, k, prog, e)
end

export buildAdvancedProcessTree

end
