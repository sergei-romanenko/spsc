package spsc

import SmallLanguage._

// The operational semantics of interpreter is
// normal-order graph reduction to weak head normal form.
class Interpreter (program: Program) {
  
  def eval(t: Term): Term = lazyEval(t) match {
    case Constructor(name, args) => 
      Constructor(name, args.map(eval))
    // TODO: assertion failed?
    case _ => throw new RuntimeException("Internal Error")
  }
  
  private def lazyEval(t: Term): Term = t match {

    case c: Constructor => c
    
    case FCall(name, args)  => 
      lazyEval(unfoldFCall(name, args))

    case GCall(name, arg0, args) =>
      lazyEvalGCall(name, arg0, args)

    case t: Term =>
      illegalTerm(t)
  }

  def lazyEvalGCall(name: String, t: Term, args: List[Term]) : Term = t match {

    case Constructor(cname, cargs) => {
      val gFunction = program.getGFunction(name, cname)
      val substitution: Map[Variable, Term] =
        Map() ++  ((gFunction.arg0.args zip cargs) ::: (gFunction.args zip args))      
      lazyEval(apllySubstitution(gFunction.term, substitution))
    }
    
    case FCall(name1, args1) => 
      lazyEvalGCall(name, unfoldFCall(name1, args1), args)

    case GCall(name1, t1, args1) =>
      lazyEvalGCall(name, lazyEvalGCall(name1, t1, args1), args)

    case t : Term =>
      illegalTerm(t)
  }
  
  def unfoldFCall(name: String, args: List[Term]): Term = {
    val fFunction = program.getFFunction(name)
    val substitution: Map[Variable, Term] = Map() ++  (fFunction.args zip args)
    apllySubstitution(fFunction.term, substitution)    
  }
  
  def apllySubstitution(term: Term, map: Map[Variable, Term]): Term = term match {
    case v: Variable => 
      map(v)
    case Constructor(name, args) => 
      Constructor(name, args.map(apllySubstitution(_, map)))
    case FCall(name, args) => 
      FCall(name, args.map(apllySubstitution(_, map)))
    case GCall(name, arg0, args) => 
      GCall(name, apllySubstitution(arg0, map), args.map(apllySubstitution(_, map)))
  }
  
  def illegalTerm(t: Term): Term = {
    throw new IllegalArgumentException(t + " is encoutered in passed expression. This term contains vars.")
  }
}