module ProcessTree_Tests

using Test
using SPSC.SLanguage
using SPSC.ProcessTree

@info "Testing ProcessTree"

@testset "Building process tree" begin

    t = Tree(Var("r")::Exp)
    r = t.root
    b1 = Branch(Var("m1"), nothing)
    b2 = Branch(Var("m2"), nothing)
    addChildren(t, r, Branch[b1, b2])
    m1 = r.children[1]
    m2 = r.children[2]
    addChildren(t, m1, [Branch(Var("n"), nothing)])
    replaceSubtree(t, m2, Var("x"))

    @test string(t) ==
        "{0:(r,nothing,nothing,[1,2]),1:(m1,nothing,0,[3]),3:(n,nothing,1,[]),2:(x,nothing,0,[])}"

    @test [n.nodeId for n in nodes(t)] == [0,1,3,2]

    @test [n.nodeId for n in leaves(t)] == [3,2]
end

end
