-- Most General Unifier
module MGU where

import SLanguage
import SLanguageShow
import Algebra

import qualified Data.Map as M

data Uni = Uni Exp (M.Map Name Exp) (M.Map Name Exp)
             deriving Show

--case class Gen(t: Term, m1: Map[Var, Term], m2: Map[Var, Term])

--  def msg(t1: Term, t2: Term): Gen = {
--    val v = freshVar()
--    var g = Gen(v, Map(v -> t1), Map(v -> t2))
--    var exp = g.t
--    do {exp = g.t; g = commonSubst(commonFun(g))} while (exp != g.t)
--    g
--  }

--  def commonFun(g: Gen): Gen = {
--    for (v <- g.m1.keys) (g.m1(v), g.m2(v)) match {
--      case (e1:CFG, e2:CFG) if shellEq(e1, e2) =>
--        val vs = e1.args map freshVar
--        val t = subst(g.t, Map(v -> e1.replaceArgs(vs)))
--        return Gen(t, g.m1 - v ++ vs.zip(e1.args), g.m2 - v ++ vs.zip(e2.args))        
--      case _ =>
--    }
--    g
--  }

mergeableKeyPairs m1 m2 =
  [ (k1,k2) | (k1,e1) <- M.assocs m1, (k2,e2) <- M.assocs m1, k1 < k2,
               e1 == e2, m2 M.! k2 == m2 M.! k2]

--m1 = M.fromList [(1,1),(2,2),(3,1),(4,2)]
--m2 = M.fromList [(1,10),(2,20),(3,10),(4,20)]
--t = mergeableKeyPairs m1 m2

mergeSubexp u @ (Uni e m1 m2) =
  case mergeableKeyPairs m1 m2 of
    [] -> u
    [(k1, k2)] ->
      let e' = substExp (M.fromList[ (k1, Var k2) ]) e
          m1' = M.delete k1 m1
          m2' = M.delete k1 m2
      in Uni e' m1' m2'

--u = Uni (Call Ctr "f" [Var "x", Var "y"])
--        (M.fromList [("x", Var "1"), ("y", Var "2"), ("z", Var "1")])
--        (M.fromList [("x", Var "10"), ("y", Var "20"), ("z", Var "10")])
--u1 = mergeSubexp u