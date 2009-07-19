module Driving where

import qualified Data.Map as Map

import SLanguage
import SLanguageShow
import SParsers
import Algebra

data ProcessTree
  = PTVar Name
  | PTCtr Name [ProcessTree]
  | PTStep Exp ProcessTree
      deriving Show

--  def driveExp(expr: Term): List[(Term, Contraction)] = expr match {
--    case GCall(name, Ctr(cname, cargs) :: args) =>
--      val g = p.g(name, cname)  
--      List((subst(g.term, Map((g.p.args:::g.args) zip (cargs:::args): _*)), null))
--    case gCall @ GCall(name, (v : Var) :: args) => 
--      for (g <- p.gs(name); fp = freshPat(g.p); cons = Ctr(fp.name, fp.args)) 
--        yield driveExp(subst(gCall, Map(v -> cons))) match 
--          {case (k, _) :: _ => (k, Contraction(v, fp))} 
--    case GCall(name, args) => 
--      driveExp(args(0)) map {case (k, v) => (GCall(name, k :: args.tail), v)}
--    case Let(term, bs) => (term, null) :: bs.map {case (_, v) => (v, null)}

lookupF prog name =
  let Program rules = prog in
  head [ (params, body) | FRule name' params body <- rules, name == name' ]

lookupGC prog name cname =
  let Program rules = prog in
  head [ (cparams, params, body) |
    GRule name' (Pat cname' cparams) params body <- rules,
    name == name',
    cname == cname' ]

lookupG prog name =
  let Program rules = prog in
  [ (cparams, params, body) |
    GRule name' (Pat cname' cparams) params body <- rules,
    name == name' ]

drive prog e =
  case e of
    Var name -> PTVar name
    Call Ctr name args ->
      PTCtr name (map (drive prog) args)
    Call FCall name args ->
      let (params, body) = lookupF prog name
          p2a = Map.fromList(params `zip` args)
          body' = substExp p2a body
      in PTStep body' $ drive prog body'
    Call GCall name (Call Ctr cname cargs : args) ->
      let (cparams, params, body) = lookupGC prog name cname
          cp2ca = Map.fromList(cparams `zip` cargs)
          p2a = Map.fromList(params `zip` args)
          body' = substExp (cp2ca `Map.union` p2a) body
      in PTStep body' $ drive prog body'
--    Call GCall name (Var vname : args) ->
--      let rules = lookupG prog name
--      
--      for (g <- p.gs(name); fp = freshPat(g.p); cons = Ctr(fp.name, fp.args)) 
--        yield driveExp(subst(gCall, Map(v -> cons))) match 
--          {case (k, _) :: _ => (k, Contraction(v, fp))} 

      
x1 = pProg "f(x)=x;" `drive` pExp "f(A(z))"

x2 = pProg "gAddAcc(Z,y)=y;gAddAcc(S(x),y)=gAddAcc(x,S(y));"
     `drive`
     pExp "gAddAcc(S(S(Z)), Z)"