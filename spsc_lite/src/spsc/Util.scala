package spsc;

import scala.util.parsing.input.CharArrayReader

object Util {
  def applySub(term: Term, map: Map[Variable, Term]): Term = term match {
    case v: Variable => 
      if (map.contains(v)) map(v) else v
    case Constructor(name, args) => 
      Constructor(name, args.map(applySub(_, map)))
    case FCall(name, args) => 
      FCall(name, args.map(applySub(_, map)))
    case GCall(name, arg0 :: args) => 
      GCall(name, applySub(arg0, map) :: args.map(applySub(_, map)))
  }
  
  def equiv(term1: Term, term2: Term): Boolean = inst(term1, term2) && inst(term2, term1)
  def inst(term1: Term, term2: Term): Boolean = sub(term1, term2).isDefined
  
  def sub(term1: Term, term2: Term): Option[Map[Variable, Term]] = {
    var map = Map[Variable, Term]()
    def walk(t1: Term, t2: Term): Boolean = (t1, t2) match {
      case (v1: Variable, _) => map.get(v1) match {
        case None => map = map.update(v1, t2); true
        case Some(t) => t == t2 
      }
      case (Constructor(n1, args1), Constructor(n2, args2)) =>
        n1 == n2 && ((args1 zip args2) forall {case (a, b) => walk(a, b)})
      case (FCall(n1, args1), FCall(n2, args2)) =>
        n1 == n2 && ((args1 zip args2) forall {case (a, b) => walk(a, b)})
      case (GCall(n1, args1), GCall(n2, args2)) =>
        n1 == n2 && ((args1 zip args2) forall {case (a, b) => walk(a, b)})
      case _ => false
    }
    if (walk(term1, term2)) Some(map.filter {case (a, b) => a == b}) else None
  }
  
  def programFromString(input: String) = { 
    SLanguageParsers.parseProgram(new CharArrayReader(input.toCharArray))
  }
}
