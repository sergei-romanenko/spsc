module ShowUtil_Test

using Test
using SPSC.SLanguage
using SPSC.ShowUtil

@info "Testing SLanguage"

@testset "Show Exp" begin
    @test "x" == string(Var("x"))
    @test "C(x,y)" == string(Call(Ctr(), "C", [Var("x"), Var("y")]))
    @test "C(x)" == string(Call(Ctr(), "C", [Var("x")]))
    @test "C" == string(Call(Ctr(), "C", []))
    @test "f(x,y)" == string(Call(FCall(), "f", [Var("x"), Var("y")]))
    @test "g(x,y)" == string(Call(GCall(), "g", [Var("x"), Var("y")]))
    @test "let x=a,y=b in x" ==
        string(Let(Var("x"), [Binding("x", Var("a")), Binding("y", Var("b"))]))
    @test "let x=a in x" == string(Let(Var("x"), [Binding("x", Var("a"))]))
    @test "let  in x" == string(Let(Var("x"), []))
end

@testset "Show Rule" begin
    @test "f(x,y)=x;" == string(FRule("f", ["x", "y"], Var("x")))
    @test "f(x)=x;" == string(FRule("f", ["x"], Var("x")))
    @test "f()=C;" == string(FRule("f", [], Call(Ctr(), "C", [])))
    @test "g(C(x,y),z)=z;" == string(GRule("g", "C", ["x", "y"], ["z"], Var("z")))
end

@testset "Show Program" begin
    @test "f()=A;f1()=A1;" ==
        string(Program([
            FRule("f", [], Call(Ctr(), "A", [])),
            FRule("f1", [], Call(Ctr(), "A1", []))]))
    @test
end
    
end
