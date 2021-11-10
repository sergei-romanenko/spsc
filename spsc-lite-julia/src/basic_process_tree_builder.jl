module BasicProcessTreeBuilder

using SPSC.SLanguage
using SPSC.Algebra
using SPSC.ProcessTree

struct DrivingEngine
    nameGen::NameGen
    fRules::Dict{Name,FRule}
    gRules::Dict{Name,Vector{GRule}}
    gcRule::Dict{Tuple{Name,Name},GRule}
end

function DrivingEngine(ng::NameGen, prog::Program)
    fRules = Dict{Name,FRule}()
    gRules = Dict{Name,Vector{GRule}}()
    gcRules = Dict{Tuple{Name,Name},GRule}()

    for rule in prog.rules
        name = rule.name
        if rule isa FRule
            fRules[name] = rule
        elseif rule isa GRule
            if name in keys(gRules)
                push!(gRules[name], rule)
            else
                gRules[name] = [rule]
            end
            gcRules[(rule.name, rule.cname)] = rule
        end

    end
    DrivingEngine(ng, fRules, gRules, gcRules)
end

function drivingStep(d::DrivingEngine, e::Exp)
    error("Invalid Exp type")
end

function drivingStep(d::DrivingEngine, e::CFG)::Vector{Branch}
    if isCtr(e)
        return [Branch(arg, nothing) for arg in e.args]
    elseif e.kind == FCall
        rule = d.fRules[e.name]
        p2a = Dict(zip(rule.params, e.args))
        body = applySubst(p2a, rule.body)
        return [Branch(body, nothing)]
    elseif e.kind == GCall
        arg1 = e.args[1]
        args = e.args[2:end]
        if isCtr(arg1)
            cname = arg1.name
            cargs = arg1.args
            rule = d.gcRule[(e.name, cname)]
            p2a = Dict(Iterators.flatten(
                (zip(rule.cparams, cargs), zip(rule.params, args))))
            body = applySubst(p2a, rule.body)
            return [Branch(body, nothing)]
        elseif arg1 isa Var
            rules = d.gRules[e.name]
            return [ driveBranch(d, e, rule) for rule in rules ]
        else
            branches = drivingStep(d, arg1)
            return [ Branch(
                CFG(GCall, e.name, append!(Exp[b.e], args)), b.contr)
                    for b in branches]
        end
    end
end

function drivingStep(d::DrivingEngine, e::Let)
    return append!([Branch(e.body, nothing)],
        [Branch(b.e, nothing) for b in e.bindings])
end

function driveBranch(d::DrivingEngine, e::Exp, rule::Rule)
    vname = e.args[1].name
    cname = rule.cname
    # params = rule.params
    cparams = freshNameList(d.nameGen, length(rule.cparams))
    cargs = [Var(vn)::Exp for vn in cparams]
    vname2ctr = Dict{Name,Exp}(vname => CFG(Ctr, cname, cargs))
    e1 = applySubst(vname2ctr, e)
    branches = drivingStep(d, e1)
    e2 = branches[1].e
    return Branch(e2, Contraction(vname, cname, cparams))
end

struct BasicPTBuilder
    d::DrivingEngine
    tree::Tree
end

# The parts common to the basic and advanced supercompilers.

# If beta `instOf` alpha, we generalize beta by introducing
# a let-expression, in order to make beta the same as alpha
# (modulo variable names).

function loopBack(bld::BasicPTBuilder, beta::Node, alpha::Node)
    subst = matchAgainst(alpha.e, beta.e)
    ks = collect(keys(subst))
    sort!(ks)
    bindings = [Binding(k, subst[k]) for k in ks]
    letExp = Let(alpha.e, bindings)
    replaceSubtree(bld.tree, beta, letExp)
end

# This function applies a driving step to the node's expression,
# and, in general, adds children to the node.

function expandNode(bld::BasicPTBuilder, beta::Node)
    branches = drivingStep(bld.d, beta.e)
    addChildren(bld.tree, beta, branches)
end

# Basic supercompiler process tree builder

function buildStep(bld::BasicPTBuilder, beta::Node)
    # This method is overridden in the advanced version of
    # the process tree builder.
    alpha = findMoreGeneralAncestor(beta)
    if alpha isa Node
        loopBack(bld, beta, alpha)
    else
        expandNode(bld, beta)
    end
end

function buildProcessTree(bld::BasicPTBuilder, k::Int64)
    # Specifying k = -1 results in an unlimited building loop.
    while true
        k > 0 || break
        k -= 1
        beta = findUnprocessedNode(bld.tree)
        beta isa Node || break
        buildStep(bld, beta)
    end
end

function buildBasicProcessTree(ng::NameGen, k::Int64, prog::Program, e::Exp)::Tree
    d = DrivingEngine(ng, prog)
    bld = BasicPTBuilder(d, Tree(e))
    buildProcessTree(bld, k)
    return bld.tree
end

export DrivingEngine, drivingStep, buildBasicProcessTree

end
