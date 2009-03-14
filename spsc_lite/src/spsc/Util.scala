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
    def walk(t1: Term, t2: Term): Boolean = t1 match {
      case v1: Variable => map.get(v1) match {
        case None => map = map.update(v1, t2); true
        case Some(t) => t == t2 
      }
      case _ => t1.productPrefix == t2.productPrefix && 
        t1.name == t2.name && ((t1.args zip t2.args) forall {case (a, b) => walk(a, b)})
    }
    if (walk(term1, term2)) Some(map.filter {case (a, b) => a == b}) else None
  }
  
  def programFromString(input: String) = { 
    SLanguageParsers.parseProgram(new CharArrayReader(input.toCharArray))
  }
}
