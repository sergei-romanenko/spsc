module Algebra_Tests

using Test
using SPSC.SLanguage
using SPSC.SParsers
using SPSC.Algebra

@info "Testing Algebra"

function runTheSameFunctor(e1::String, e2::String)::Bool
  theSameFunctor(parseExpr(e1), parseExpr(e2))
end

@testset "theSameFunctor" begin
  @test runTheSameFunctor("A", "A")
  @test !runTheSameFunctor("A", "A(x)")
  @test runTheSameFunctor("f(A)", "f(B)")
  @test runTheSameFunctor("g(A)", "g(B)")
  @test !runTheSameFunctor("A" , "B")
  @test !runTheSameFunctor("A", "f()")
end

@testset "Testing ==" begin
    @test Var("x") == Var("x")
    @test Var("x") != Var("y")
    @test CFG(Ctr(), "A", []) == CFG(Ctr(), "A", [])
    @test CFG(Ctr(), "A", []) != CFG(Ctr(), "B", [])
    @test [] == []
    @test [Var("x")] == [Var("x")]
    @test [Var("x")] != [Var("y")]
    @test [Var("x")] != [Var("x"), Var("z")]
    @test CFG(Ctr(), "A", [Var("x")]) == CFG(Ctr(), "A", [Var("x")])
    @test CFG(Ctr(), "A", [Var("x")]) != CFG(Ctr(), "A", [Var("y")])
end

function matchOK(e1, e2, expected)
  actual = "*"
  s = matchAgainst(parseExpr(e1), parseExpr(e2))
  if s isa Subst
    kvs = ["$key->$(s[key]);" for key in sort(collect(keys(s)))]
    actual = string(kvs...)
        end
  @test expected == actual
end

function matchNothing(e1, e2)
  @test matchAgainst(parseExpr(e1), parseExpr(e2)) isa Nothing
end

@info "Testing Algebra"

@testset "vars" begin
  @test ["x", "y", "a"] == vars(parseExpr("A(x,B(y,x),a)"))
end

@testset "applySubst" begin
  e1 = parseExpr("E1")
  e2 = parseExpr("E2")
  e = parseExpr("Cons(x1, Cons(x2, Cons(x3, Nil)))")
  s = Dict{Name,Exp}("x1" => e1, "x2" => e2)
  @test "Cons(E1,Cons(E2,Cons(x3,Nil)))" == string(applySubst(s, e))
end

@testset "matchAgainst" begin
  matchOK("x", "S(Z)", "x->S(Z);")
  matchNothing("Z", "x")
  matchOK("C(x,y)", "C(A,B)", "x->A;y->B;")
    matchNothing("C(x,y)", "D(A,B)")
  matchNothing("C(x,y)", "f(A,B)")
  matchOK("C(x,x)", "C(A,A)", "x->A;")
  matchNothing("C(x,y)", "C(A,B,C)")
  matchNothing("C(x,y,z)", "C(A,B)")
end

testEquiv(e1::String, e2::String, expected::Bool) =
  @test expected == equiv(parseExpr(e1), parseExpr(e2))

@testset "equiv" begin
  testEquiv("gA(fB(x,y),C)", "gA(fB(a,b),C)", true)
  testEquiv("gA(fB(x,y),x)", "gA(fB(a,a),b)", false)
end

end
