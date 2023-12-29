module ResidualProgramGenerator_Tests

using Test
using SPSC.SLanguage
using SPSC.SParsers
using SPSC.Algebra
using SPSC.BasicProcessTreeBuilder
using SPSC.AdvancedProcessTreeBuilder
using SPSC.ResidualProgramGenerator

# Sample programs

const pAdd = "gAdd(Z,y)=y;gAdd(S(x),y)=S(gAdd(x,y));"
const pAddAcc = "gAddAcc(Z,y)=y;gAddAcc(S(x),y)=gAddAcc(x,S(y));"

#---- Basic supercompiler

function runBasicScp(prog::Program, e::Exp)
    ng = NameGen("v", 100)
    tree = buildBasicProcessTree(ng, 100, prog, e)
    return genResidualProgram(tree)
end

function basicScpOK(prog::String, e::String, expected::String)
    (resPr, resExp) = runBasicScp(parseProg(prog), parseExpr(e))
    res_s = "$(resPr)/$(resExp)"
    @test expected == res_s
end

@info "Testing supercompilers"

@testset "Basic supercompiler" begin
    basicScpOK("", "a", "/a")
    basicScpOK("", "C(a,b)", "/C(a,b)")
    basicScpOK(pAdd, "gAdd(a, b)",
        "gAdd1(Z,b)=b;gAdd1(S(v100),b)=S(gAdd1(v100,b));/gAdd1(a,b)")
    basicScpOK(pAdd, "gAdd(gAdd(a,b),c)",
        "gAdd2(Z,c)=c;gAdd2(S(v101),c)=S(gAdd2(v101,c));" *
        "gAdd1(Z,b,c)=gAdd2(b,c);gAdd1(S(v100),b,c)=S(gAdd1(v100,b,c));/gAdd1(a,b,c)")
    basicScpOK(pAddAcc, "gAddAcc(a, b)",
        "gAddAcc1(Z,b)=b;gAddAcc1(S(v100),b)=gAddAcc1(v100,S(b));/gAddAcc1(a,b)")
end

#---- Advanced supercompiler

function runAdvancedScp(prog::Program, e::Exp)
    ng = NameGen("v", 100)
    tree = buildAdvancedProcessTree(ng, 100, prog, e)
    return genResidualProgram(tree)
end

function advancedScpOK(prog::String, e::String, expected::String)
    (resPr, resExp) = runAdvancedScp(parseProg(prog), parseExpr(e))
    res_s = "$(resPr)/$(resExp)"
    @test expected == res_s
end

@testset "Advanced supercompiler" begin
    advancedScpOK(pAdd, "gAdd(a, b)",
        "gAdd1(Z,b)=b;gAdd1(S(v100),b)=S(gAdd1(v100,b));/gAdd1(a,b)")
    advancedScpOK(pAdd, "gAdd(a, a)",
        "gAdd1(Z,v103)=v103;gAdd1(S(v104),v103)=S(gAdd1(v104,v103));/gAdd1(a,a)")
    advancedScpOK(pAdd, "gAdd(gAdd(a,b),c)",
        "gAdd2(Z,c)=c;gAdd2(S(v101),c)=S(gAdd2(v101,c));" *
        "gAdd1(Z,b,c)=gAdd2(b,c);gAdd1(S(v100),b,c)=S(gAdd1(v100,b,c));/gAdd1(a,b,c)")
    advancedScpOK(pAddAcc, "gAddAcc(a, b)",
        "gAddAcc1(Z,b)=b;gAddAcc1(S(v100),b)=gAddAcc1(v100,S(b));/gAddAcc1(a,b)")
    advancedScpOK(pAddAcc, "gAddAcc(a, a)",
        "gAddAcc1(Z,v103)=v103;gAddAcc1(S(v104),v103)=gAddAcc1(v104,S(v103));/gAddAcc1(a,a)")
    advancedScpOK(pAddAcc, "gAddAcc(gAddAcc(a,b),c)",
        "gAddAcc2(Z,c)=c;gAddAcc2(S(v101),c)=gAddAcc2(v101,S(c));" *
        "gAddAcc1(Z,b,c)=gAddAcc2(b,c);gAddAcc1(S(v100),b,c)=gAddAcc1(v100,S(b),c);" *
        "/gAddAcc1(a,b,c)")
    advancedScpOK("f(x)=f(S(x));", "f(a)",
        "f1(a)=f1(S(a));/f1(a)")
    advancedScpOK("f(x)=g(f(x));g(A)=B;", "f(a)",
        "g2(A)=B;f1(a)=g2(f1(a));/f1(a)")
    advancedScpOK("g1(C(x))= B;g2(B,x)=x;", "g2(g1(x),x)",
        "g21(C(v100))=C(v100);/g21(x)")
end

end
