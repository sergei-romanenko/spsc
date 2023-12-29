module AdvancedProcessTreeBuilder_Tests

using Test
using SPSC.SLanguage
using SPSC.SParsers
using SPSC.Algebra
# using SPSC.ProcessTree
using SPSC.AdvancedProcessTreeBuilder

function buildPrTree(prog::Program, e::Exp, k::Int64)
    ng = NameGen("v", 100)
    return buildAdvancedProcessTree(ng, k, prog, e)
end

function buildPrTree1OK(prog::String, e::String, expected::String)
    tree = buildPrTree(parseProg(prog), parseExpr(e), 1)
    @test expected == string(tree)
end

function buildPrTreeOK(prog::String, e::String, expected::String)
    tree = buildPrTree(parseProg(prog), parseExpr(e), 100)
    @test expected == string(tree)
end

const pAdd = "gAdd(Z,y)=y;gAdd(S(x),y)=S(gAdd(x,y));"
const pAddAcc = "gAddAcc(Z,y)=y;gAddAcc(S(x),y)=gAddAcc(x,S(y));"

@info "Testing advanced process tree builder"

@testset "buildPrTree1" begin
    buildPrTree1OK("", "x",
        "{0:(x,nothing,nothing,[])}")
    buildPrTree1OK("", "S(Z)",
        "{0:(S(Z),nothing,nothing,[1]),1:(Z,nothing,0,[])}")
    buildPrTree1OK(pAddAcc, "gAddAcc(S(Z), Z)",
        "{0:(gAddAcc(S(Z),Z),nothing,nothing,[1]),1:(gAddAcc(Z,S(Z)),nothing,0,[])}")
    buildPrTree1OK(pAdd, "gAdd(a, b)", "{" *
        "0:(gAdd(a,b),nothing,nothing,[1,2]),1:(b,a=Z,0,[])," *
        "2:(S(gAdd(v100,b)),a=S(v100),0,[])}")
    buildPrTree1OK(pAdd, "gAdd(gAdd(a,b),c)", "{" *
        "0:(gAdd(gAdd(a,b),c),nothing,nothing,[1,2]),1:(gAdd(b,c),a=Z,0,[])," *
        "2:(gAdd(S(gAdd(v100,b)),c),a=S(v100),0,[])}")
end
    
@testset "buildPrTree" begin
    buildPrTreeOK(pAddAcc, "gAddAcc(a,b)", "{" *
        "0:(gAddAcc(a,b),nothing,nothing,[1,2]),1:(b,a=Z,0,[])," *
        "2:(let a=v100,b=S(b) in gAddAcc(a,b),a=S(v100),0,[3,4,5])," *
        "3:(gAddAcc(a,b),nothing,2,[]),4:(v100,nothing,2,[])," *
        "5:(S(b),nothing,2,[6]),6:(b,nothing,5,[])}")
    buildPrTreeOK(pAddAcc, "gAddAcc(S(Z), Z)", "{" *
        "0:(gAddAcc(S(Z),Z),nothing,nothing,[1]),1:(gAddAcc(Z,S(Z)),nothing,0,[2])," *
        "2:(S(Z),nothing,1,[3]),3:(Z,nothing,2,[])}")
    buildPrTreeOK(pAdd, "gAdd(a, b)", "{" *
        "0:(gAdd(a,b),nothing,nothing,[1,2]),1:(b,a=Z,0,[])," *
        "2:(S(gAdd(v100,b)),a=S(v100),0,[3]),3:(gAdd(v100,b),nothing,2,[])}")
    buildPrTreeOK(pAdd, "gAdd(gAdd(a,b),c)", "{" *
        "0:(gAdd(gAdd(a,b),c),nothing,nothing,[1,2]),1:(gAdd(b,c),a=Z,0,[3,4])," *
        "3:(c,b=Z,1,[]),4:(S(gAdd(v101,c)),b=S(v101),1,[5])," *
        "5:(gAdd(v101,c),nothing,4,[]),2:(gAdd(S(gAdd(v100,b)),c),a=S(v100),0,[6])," *
        "6:(S(gAdd(gAdd(v100,b),c)),nothing,2,[7]),7:(gAdd(gAdd(v100,b),c),nothing,6,[])}")
    buildPrTreeOK(pAdd, "gAdd(a, a)", "{" *
        "0:(let v102=a,v103=a in gAdd(v102,v103),nothing,nothing,[4,5,6])," *
        "4:(gAdd(v102,v103),nothing,0,[7,8]),7:(v103,v102=Z,4,[])," *
        "8:(S(gAdd(v104,v103)),v102=S(v104),4,[9]),9:(gAdd(v104,v103),nothing,8,[])," *
        "5:(a,nothing,0,[]),6:(a,nothing,0,[])}")
    buildPrTreeOK("f(x) = g(f(x));g(A) = B;", "f(a)", "{" *
        "0:(f(a),nothing,nothing,[1]),1:(let v101=f(a) in g(v101),nothing,0,[2,3])," *
        "2:(g(v101),nothing,1,[4]),4:(B,v101=A,2,[]),3:(f(a),nothing,1,[])}")
    end

end
