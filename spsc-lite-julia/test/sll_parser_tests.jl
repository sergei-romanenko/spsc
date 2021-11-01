module SParsers_Test

using Test
using ParserCombinator
using SPSC.SLanguage
using SPSC.SParsers

function identOK(p, expected::String, input::String)
    tokens = parse_one(input, p + Eos())
    @test expected == tokens[1]
end

function parseOK(p, expected::String, input::String)
    tokens = parse_one(input, p + Eos())
    @test expected == string(tokens[1])
end

function exprOK(expected::String, input::String)
    parseOK(expr, expected, input)
end

function programOK(expected::String, input::String)
    parseOK(program, expected, input)
end

function exc(p, input)
    @test_throws ParserException("cannot parse") parse_one(input, p + Eos())
end

@info "Testing SParsers"

@testset "Parse ident" begin
    identOK(lIdent, "aAa9b", "aAa9b")
    identOK(uIdent, "Aaa9b", "Aaa9b")
    identOK(fIdent, "fAa9b", "fAa9b")
    identOK(gIdent, "gAa9b", "gAa9b")
    exc(lIdent, "Aa")
    exc(uIdent, "aa")
    exc(lIdent, "A*a")
end

@testset "Parse exp" begin
    exprOK("x", "x")
    exprOK("fA", "fA")
    exprOK("gA", "gA")
    exprOK("A(x,y)", "A(x,y)")
    exprOK("A", "A()")
    exprOK("A", "A")
    exprOK("fA(x,y)", "fA(x,y)")
    exprOK("fA()", "fA()")
    exprOK("gA(x,y)", "gA(x,y)")
    exc(expr, "gA()")
end

@testset "Parse program" begin
    programOK("", "")
    programOK("f(x)=A;", "f(x)=A;")
    programOK("f(x)=A;f1(x)=A1;", "f(x)=A;f1(x)=A1;")
    programOK("f()=f();", "f()=f();")
    programOK("g(C)=A;", "g(C)=A;")
    programOK("g(C(x))=A;", "g(C(x))=A;")
    programOK("g(C(x))=A;", "g(C(x))=A;")
    programOK("g(C(x,y),z)=A;", "g(C(x,y),z)=A;")
    programOK("g(C,y)=A;", "g(C,y)=A;")

    programOK("g(C(x,y),z)=A;", " g ( C ( x , y ) , z ) = A ; ")
end

end
