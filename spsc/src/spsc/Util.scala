package spsc;

import SmallLanguage._

object Util {
  def applySubstitution(term: Term, map: Map[Variable, Term]): Term = term match {
  case v: Variable => 
    if (map.contains(v)) map(v) else v
  case Constructor(name, args) => 
    Constructor(name, args.map(applySubstitution(_, map)))
  case FCall(name, args) => 
    FCall(name, args.map(applySubstitution(_, map)))
  case GCall(name, arg0, args) => 
    GCall(name, applySubstitution(arg0, map), args.map(applySubstitution(_, map)))
}
}
