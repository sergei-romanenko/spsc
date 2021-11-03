module HE_Tests

using Test
using SPSC.SLanguage
using SPSC.SParsers
using SPSC.HE

runHE(e1::String, e2::String)::Bool =
  he(parseExpr(e1), parseExpr(e2))

@info "Testing HE"

@testset "he vv vf fv" begin
  @test runHE("v1", "v2")
  @test runHE("v1", "f(v2)")
  @test !runHE("f(v2)", "v1")
end

@testset "he diving" begin
  @test runHE("F(v1)", "G(v0, F(H(v2)))")
end

@testset "he coupling" begin
  @test runHE("F(v1, G(v2))", "F(H(w1), G(w2))")
  @test !runHE("F(v1)", "G(w1)")
end

end
