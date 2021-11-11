module ProcessTree

using SPSC.SLanguage
using SPSC.ShowUtil
using SPSC.Algebra

struct Contraction
    vname::Name
    cname::Name
    cparams::Params
end

const OptContraction = Union{Contraction,Nothing}

function Base.show(io::IO, c::Contraction)
    print(io, c.vname)
    print(io, "=")
    print(io, c.cname)
    if !isempty(c.cparams)
        print(io, "(")
        print(io, join(c.cparams, ","))
        print(io, ")")
    end
end

const NodeId = Int64

mutable struct Node
    # The constructor is supposed to be called via ProcessTree#newNode only.
    # nodeId is only used for unit testing purposes.
    nodeId::NodeId
    e::Exp
    contr::OptContraction
    parent::Union{Node,Nothing}
    children::Vector{Node}
end

const OptNode = Union{Node,Nothing}

function Base.show(io::IO, n::Node)
    print(io, n.nodeId)
    print(io, ":(")
    print(io, n.e)
    print(io, ",")
    print(io, n.contr)
    print(io, ",")
    print(io, n.parent isa Node ? n.parent.nodeId : nothing)
    print(io, ",[")
    print(io, join([string(child.nodeId) for child in n.children], ","))
    print(io, "])")
end

ancestors(n::Node)::Channel{Node} = Channel{Node}() do c
    a = n.parent
    while a isa Node
        put!(c, a)
        a = a.parent
    end
    close(c)
end

function funcAncestor(n::Node)::OptNode
    for a in ancestors(n)
        equiv(n.e, a.e) && return a
    end
    return nothing
end

function findMoreGeneralAncestor(n::Node)::OptNode
    for a in ancestors(n)
        isFGCall(a.e) && instOf(n.e, a.e) && return a
    end
    return nothing
end

isProcessed(n::Node, e::Var)::Bool = true

function isProcessed(n::Node, e::CFG)::Bool
    e.kind == Ctr && return isempty(e.args)
    return funcAncestor(n) !== nothing
end

isProcessed(n::Node, e::Let)::Bool = false

isProcessed(n::Node)::Bool = isProcessed(n, n.e)

subtreeNodes(n::Node)::Channel{Node} = Channel{Node}() do c
    put!(c, n)
    for child in n.children
        for sn in subtreeNodes(child)
            put!(c, sn)
        end
    end
end

isLeaf(n::Node) = isempty(n.children)

subtreeLeaves(n::Node)::Channel{Node} = Channel{Node}() do c
    for sn in subtreeNodes(n)
        if isLeaf(sn) put!(c, sn) end
    end
end

mutable struct Tree
    freshNodeId::NodeId
    root::Node
    # By convention, the root node's id is 0.
    Tree(e::Exp) = new(1, Node(0, e, nothing, nothing, Vector{Node}[]))
end

nodes(tree::Tree) = subtreeNodes(tree.root)

function Base.show(io::IO, tree::Tree)
    print(io, "{")
    print(io, join([string(n) for n in nodes(tree)], ","))
    print(io, "}")

end

# The tree is supposed to be non-empty.
leaves(tree::Tree) = subtreeLeaves(tree.root::Node)

function findUnprocessedNode(tree::Tree)::OptNode
    for n in leaves(tree)
        isProcessed(n) || return n
    end
    return nothing
end

function isFuncNode(tree::Tree, node)
    for leaf in leaves(tree)
        node == funcAncestor(leaf) && return true
    end
    return false
end

function newNode(tree::Tree, e::Exp, contr::OptContraction, parent::OptNode, children::Vector{Node})::Node
    i = tree.freshNodeId
    tree.freshNodeId += 1
    Node(i, e, contr, parent, children)
end

struct Branch
    e::Exp
    contr::OptContraction
end

function addChildren(tree::Tree, n::Node, branches::Vector{Branch})::Nothing
    children = [newNode(tree, b.e, b.contr, n, Node[]) for b in branches]
    append!(n.children, children)
    return
end

function replaceSubtree(tree::Tree, n::Node, e::Exp)::Nothing
    n.children = Vector{Node}[]
    n.e = e
    return
end

export Contraction, Node
export ancestors, nodes, leaves
export Tree, Branch, addChildren, replaceSubtree
export findUnprocessedNode, findMoreGeneralAncestor

end
