module ShowUtil_Test

using Test
using SPSC.SLanguage
using SPSC.ShowUtil

@info "Testing SLanguage"

@testset "Show Exp" begin
    @test "x" == string(Var("x"))
    @test "C(x,y)" == string(CFG(Ctr, "C", [Var("x"), Var("y")]))
    @test "C(x)" == string(CFG(Ctr, "C", [Var("x")]))
    @test "C" == string(CFG(Ctr, "C", []))
    @test "f(x,y)" == string(CFG(FCall, "f", [Var("x"), Var("y")]))
    @test "g(x,y)" == string(CFG(GCall, "g", [Var("x"), Var("y")]))
    @test "let x=a,y=b in x" ==
        string(Let(Var("x"), [Binding("x", Var("a")), Binding("y", Var("b"))]))
    @test "let x=a in x" == string(Let(Var("x"), [Binding("x", Var("a"))]))
    @test "let in x" == string(Let(Var("x"), []))
end

@testset "Show Rule" begin
    @test "f(x,y)=x;" == string(FRule("f", ["x", "y"], Var("x")))
    @test "f(x)=x;" == string(FRule("f", ["x"], Var("x")))
    @test "f()=C;" == string(FRule("f", [], CFG(Ctr, "C", [])))
    @test "g(C(x,y),z)=z;" == string(GRule("g", "C", ["x", "y"], ["z"], Var("z")))
end

@testset "Show Program" begin
    @test "f()=A;f1()=A1;" ==
        string(Program([
            FRule("f", [], CFG(Ctr, "A", [])),
            FRule("f1", [], CFG(Ctr, "A1", []))]))
    @test "g(C)=A;g1(C,x)=A;g2(C(x))=A;" ==
    string(Program([
        GRule("g", "C", [], [], CFG(Ctr, "A",[])),
        GRule("g1", "C", [], ["x"], CFG(Ctr, "A",[])),
        GRule("g2", "C", ["x"], [], CFG(Ctr, "A",[]))]))
end

end
