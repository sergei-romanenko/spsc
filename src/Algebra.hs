module Algebra where

import qualified Data.Map as M
import Control.Monad.State

import SLanguage

theSameFunctor (Call kind1 name1 _) (Call kind2 name2 _) =
  kind1 == kind2 && name1 == name2
theSameFunctor _ _ = False

substExp m e =
  case e of
    Var name ->
      M.findWithDefault e name m
    Call kind name args ->
      Call kind name (map (substExp m) args)

--  
--  def equiv(t1: Term, t2: Term): Boolean = inst(t1, t2) && inst(t2, t1)
--  
--  def inst(t1: Term, t2: Term): Boolean = findSubst(t1, t2) != null
--  
--  def findSubst(t1: Term, t2: Term): Map[Var, Term] = {
--    val map = scala.collection.mutable.Map[Var, Term]()
--    def walk(t1: Term, t2: Term): Boolean = (t1, t2) match {
--      case (v1: Var, _) => map.put(v1, t2) match {case None => true; case Some(t3) => t2 == t3}
--      case (e1: CFG, e2:CFG) if shellEq(e1, e2) => List.forall2(e1.args, e2.args)(walk)
--      case _ => false
--    }
--    if (walk(t1, t2)) Map(map.toList:_*).filter{case (k, v) => k != v} else null
--  }
--  
--  def vars(t: Term): List[Var] = t match {
--    case v: Var => (List(v))
--    case e: CFG => (List[Var]() /: e.args) {_ union vars(_)}
--  }
--  
--  def freshVar(x: AnyRef) = {i += 1; Var("v" + i)}; private var i = 0;
--  
--  def trivial(expr: Term): Boolean = expr match {
--    case FCall(_, _) => false
--    case GCall(_, _) => false
--    case _ => true
--  }

mkName t = "$" ++ show t

freshName :: State Int Name
freshName =
  do t <- get
     put $ t+1
     return $ mkName t

freshNameList n =
  do t <- get
     put $ t + n
     return $ map mkName [t..(t+n-1)]
