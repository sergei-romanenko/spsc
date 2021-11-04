module MSG_Tests

using Test
using SPSC.SLanguage
using SPSC.SParsers
using SPSC.Algebra
using SPSC.MSG

function msgOK(e1::String, e2::String, expected::String)
  ng = NameGen("v", 1)
  gen = msg(ng, parseExpr(e1), parseExpr(e2))
  @test expected == string(gen)
end

@info "Testing MSG"

@testset "msg: common functor" begin
  msgOK(
    "A(a1,C(a2,a3))",
    "A(b1,C(b2,b3))",
    "A(v2,C(v4,v5)) =>> {v2->a1;v4->a2;v5->a3;}{v2->b1;v4->b2;v5->b3;}")
end

@testset "msg: merge subexpr" begin
  msgOK(
    "f(a1,a2,a1)",
    "f(b1,b2,b1)",
    "f(v2,v3,v2) =>> {v2->a1;v3->a2;}{v2->b1;v3->b2;}")
    msgOK(
      "f(a,a)",
      "f(b,S(b))",
      "f(v2,v3) =>> {v2->a;v3->a;}{v2->b;v3->S(b);}")
end

end
