module SParsers

using SPSC.SLanguage
using ParserCombinator

spc = Drop(Star(Space()))

@with_pre spc begin

  lIdent = p"[a-z][A-Za-z0-9]*"
  uIdent = p"[A-Z][A-Za-z0-9]*"
  fIdent = p"f[A-Za-z0-9]*"
  gIdent = p"g[A-Za-z0-9]*"

  expr = Delayed()

  COMMA = E","
  L = E"("
  R = E")"
  EQ = E"="
  SC = E";"

  variable = lIdent > Var
  
  ctrArgList = Opt(L + StarList(expr, COMMA) + R) |> identity
  constructor = uIdent + ctrArgList > ((name, args) -> CFG(Ctr(), name, args))

  fArgList = L + StarList(expr, COMMA) + R |> identity
  fCall = fIdent + fArgList > ((name, args) -> CFG(FCall(), name, args))

  gArgList = L + PlusList(expr, COMMA) + R |> identity
  gCall = gIdent + gArgList > ((name, args) -> CFG(GCall(), name, args))
  
  expr.matcher = constructor | fCall | gCall | variable
  
  fParamList = StarList(lIdent, COMMA) |> identity
  
  fRule = fIdent + L + fParamList + R + EQ + expr + SC >
    ((name, params, body) -> FRule(name, params, body))
  
  patternArgList = Opt(L + StarList(lIdent, COMMA) + R) |> identity
  pattern = uIdent + patternArgList

  gParamList = Opt(COMMA + PlusList(lIdent, COMMA)) |> identity
  
  gRule = gIdent + L + pattern + gParamList + R + EQ + expr + SC >
    ((name, cname, cparams, params, body) -> GRule(name, cname, cparams, params, body))

  program = Star(fRule | gRule) + spc |> Program

end

function parseExpr(input::String)::Exp
  parse_one(input, expr + Eos())[1]
end

function parseProg(input::String)::Program
  parse_one(input, program + Eos())[1]
end

export lIdent, uIdent, fIdent, gIdent
export expr, program
export parseExpr, parseProg

end
