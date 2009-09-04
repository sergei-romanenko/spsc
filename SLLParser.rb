require 'rubygems'
require 'rparsec'
require 'rparsec/parsers'

require 'SLL'

include RParsec

module SLLParser
  extend Parsers

  LIdent = regexp(/[a-z][\w]*/)
  UIdent = regexp(/[A-Z][\w]*/)
  FIdent = regexp(/f[\w]*/)
  GIdent = regexp(/g[\w]*/)

  LPAR = char('(')
  RPAR = char(')')
  EQ = char('=')
  SC = char(';')

  VarExp = LIdent.map{|vname| SLL::Var.new(vname)}

  CArgList = (LPAR >> lazy{Expression}.separated(char(',')) << RPAR).optional([])

  CtrExp = sequence(UIdent, CArgList){|cname, args| SLL::Ctr.new(cname, args)}

  FArgList = LPAR >> lazy{Expression}.separated(char(',')) << RPAR

  FCallExp = sequence(FIdent, FArgList){|name, args| SLL::FCall.new(name, args)}

  GArgList = LPAR >> lazy{Expression}.separated1(char(',')) << RPAR

  GCallExp = sequence(GIdent, GArgList){|name, args| SLL::GCall.new(name, args)}

  Expression = alt(CtrExp, FCallExp, GCallExp, VarExp)

  PatParamList = (LPAR >> LIdent.separated(char(',')) << RPAR).optional([])

  Pat = sequence(UIdent, PatParamList) {|cname, cparams| [cname, cparams]}

  FParamList = LIdent.separated(char(','))

  FRuleDecl = sequence(FIdent, LPAR >> FParamList << RPAR << EQ, Expression << SC ) do
    |name, params, body| SLL::FRule.new(name, params, body)
  end

  GParamList = (char(',') >> LIdent).many

  GRuleDecl = sequence(GIdent, LPAR >> Pat , GParamList << RPAR << EQ, Expression << SC) do
    | name, pat, params, body | SLL::GRule.new(name, pat[0], pat[1], params, body)
  end

  Program = alt(FRuleDecl, GRuleDecl).many

end

