package spsc

import SmallLanguage._

// The operational semantics of interpreter is
// normal-order graph reduction to weak head normal form.
class Interpreter (program: List[Definition]) {
  
  def eval(t: Term): Term = t match {
    // C(...)
    case Constructor(name, args) => 
      Constructor(name, args.map(eval))
    
    // f(...)
    case fCall: FCall  => 
      eval(reduce(fCall))
    
    // g(C(..), ...)
    case gCall @ GCall(gname, (c: Constructor), args) =>
      eval(reduce(gCall))
    
    // g(f(...), ...)
    case gCall @ GCall(gname, fCall: FCall, args) => 
      eval(GCall(gname, reduce(fCall), args))
      
    // g(g(...), ...)
    case GCall(gname, gCall: GCall, args) =>
      eval(GCall(gname, eval(gCall), args))
      
    case t: Term => throw new IllegalArgumentException(t + " is encoutered in passed expression. This term contains vars.")
  }
  
  def reduce(call: Call): Term = call match {
    case FCall(name, args) => {
      val fFunction = program.find(_ match {
        case FFunction(fname, _, _) if (fname == name) => true; 
        case _ => false}).get.asInstanceOf[FFunction]
      val substitution: Map[Variable, Term] = Map() ++  (fFunction.args zip args)
      performSubstitution(fFunction.term, substitution)
    }
    case GCall(name, Constructor(cname, cargs), args) => {
      val gFunction = program.find(_ match {
        case GFunction(gname, Pattern(gcname, _),  _, _) if (gname == name && gcname == cname) => true; 
        case _ => false}).get.asInstanceOf[GFunction]     
      val substitution: Map[Variable, Term] = Map() ++  ((gFunction.arg0.args zip cargs) ::: (gFunction.args zip args))
      performSubstitution(gFunction.term, substitution)
    }
    case c: Call => throw new IllegalArgumentException("Internal Error. Interpreter tried to reduce following call: " + c)
  }
  
  def performSubstitution(term: Term, map: Map[Variable, Term]): Term = term match {
    case v: Variable => 
      map(v)
    case Constructor(name, args) => 
      Constructor(name, args.map(performSubstitution(_, map)))
    case FCall(name, args) => 
      FCall(name, args.map(performSubstitution(_, map)))
    case GCall(name, arg0, args) => 
      GCall(name, performSubstitution(arg0, map), args.map(performSubstitution(_, map)))
  }
}