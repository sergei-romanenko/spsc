package spsc;

import scala.util.parsing.input.CharArrayReader

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
  
  abstract sealed class FType
  case object F extends FType
  case object G extends FType
  
  def correctCalls(rawTerm: Term, program: Program): Term = {
    def cc(t: Term): Term = t match {
      case v: Variable => v
      case Constructor(name, args) => Constructor(name, args.map(cc))
      case FCall(name, args) => funType(name, program) match {
        case F => {
          assume(program.getFFunction(name).args.size == args.size, "bad call: " + t);
          FCall(name, args.map(cc))
        }
        case G => {
          assume(program.getGFunctions(name).head.args.size == args.size - 1, "bad call: " + t);
          GCall(name, cc(args.head), args.tail.map(cc))
        }
      }
      case g: GCall => throw new IllegalArgumentException("Internal error: raw term contains g-call")
    }
    cc(rawTerm)
  }
  
  private def funType(name: String, program: Program): FType = program.definitions.find(_.name == name) match {
    case None => throw new IllegalArgumentException("Function " + name + " is undefined")
    case Some(d) => d match {case f: FFunction => F; case g: GFunction => G;}
  }
  
  def programFromString(input: String) = { 
    val pr = SmallLanguageParsers.parseProgram(new CharArrayReader(input.toCharArray))
    if (pr.isEmpty) throw new IllegalArgumentException(pr.toString)
    new Program(pr.get)
  }
}
