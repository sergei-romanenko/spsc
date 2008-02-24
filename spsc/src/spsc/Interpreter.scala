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
    case fCall @ Call(_, _, CallType.F)  => 
      eval(reduce(fCall))
    
    // g(C(..), ...)
    case gCall @ Call(gname, (c: Constructor)  :: args, CallType.G) =>
      eval(reduce(gCall))
    
    // g(f(...), ...)
    case gCall @ Call(gname, (fCall @ Call(_, _, CallType.F)) :: args, CallType.G) => 
      eval(Call(gname, reduce(fCall) :: args, CallType.G))
      
    // g(g(...), ...)
    case Call(gname, (gCall @ Call(_, _, CallType.G)) :: args, CallType.G) =>
      eval(Call(gname, eval(gCall) :: args, CallType.G))
  }
  
  def reduce(call: Call): Term = call match {
    case Call(name, args, CallType.F) => {
      val definition = program.find(_.pattern match {
        case FPattern(fname, _) if (fname == name) => true; 
        case _ => false}).get
      val fpattern: FPattern = definition.pattern.asInstanceOf[FPattern]
      val substitution: Map[Term, Term] = Map() ++  (fpattern.args zip args)
      performSubstitution(definition.term, substitution)
    }
    case Call(name, Constructor(cname, cargs)::args, CallType.G) => {
      val definition = program.find(_.pattern match {
        case GPattern(gname, Constructor(gcname, _) :: _) if (gname == name && gcname == cname) => true; 
        case _ => false}).get
      val gpattern: GPattern = definition.pattern.asInstanceOf[GPattern]
      val gconsArgs = gpattern.args.head.asInstanceOf[Constructor].args
      val gArgs = gpattern.args.tail     
      val substitution: Map[Term, Term] = Map() ++  ((gconsArgs zip cargs) ::: (gArgs zip args))
      performSubstitution(definition.term, substitution)
    }
  }
  
  def performSubstitution(term: Term, map: Map[Term, Term]): Term = term match {
    case v: Variable => 
      map(v)
    case Constructor(name, args) => 
      Constructor(name, args.map(performSubstitution(_, map)))
    case Call(name, args, ctype) => 
      Call(name, args.map(performSubstitution(_, map)), ctype)
  }
}