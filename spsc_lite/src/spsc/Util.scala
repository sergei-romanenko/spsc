package spsc;

import scala.util.parsing.input.CharArrayReader

object Util {
  def sub(term: Term, map: Map[Var, Term]): Term = term match {
    case v: Var => if (map.contains(v)) map(v) else v
    case Cons(n, vs)  => Cons(n, vs.map(sub(_, map)))
    case FCall(n, vs) => FCall(n, vs.map(sub(_, map)))
    case GCall(n, vs) => GCall(n, vs.map(sub(_, map)))
  }
  
  def equiv(term1: Term, term2: Term): Boolean = inst(term1, term2) && inst(term2, term1)
  def inst(term1: Term, term2: Term): Boolean = findSub(term1, term2) != null
  
  def findSub(term1: Term, term2: Term): Map[Var, Term] = {
    var map = Map[Var, Term]()
    def walk(t1: Term, t2: Term): Boolean = (t1, t2) match {
      case (v1: Var, _) => map.get(v1) match {
        case None => map = map.update(v1, t2); true
        case Some(t) => t == t2 
      }
      case (Cons(n1, args1), Cons(n2, args2)) =>
        n1 == n2 && ((args1 zip args2) forall {case (a, b) => walk(a, b)})
      case (FCall(n1, args1), FCall(n2, args2)) =>
        n1 == n2 && ((args1 zip args2) forall {case (a, b) => walk(a, b)})
      case (GCall(n1, args1), GCall(n2, args2)) =>
        n1 == n2 && ((args1 zip args2) forall {case (a, b) => walk(a, b)})
      case _ => false
    }
    if (walk(term1, term2)) map.filter {case (a, b) => a == b} else null
  }
}
