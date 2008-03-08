package spsc

import SmallLanguage._
import Util._

// The operational semantics of the interpreter is
// the normal-order graph reduction to the weak head normal form.
class Interpreter (program: Program) {
  
  def eval(t: Term): Term = {
    val lazyResult = lazyEval(t) 
    Constructor(lazyResult.name, lazyResult.args.map(eval))
  }
  
  private def lazyEval(t: Term): Constructor = t match {

    case c: Constructor => c
    
    case FCall(name, args)  => 
      lazyEval(unfoldFCall(name, args))

    case GCall(name, arg0, args) =>
      lazyEvalGCall(name, arg0, args)

    case t: Term =>
      illegalTerm(t)
  }

  def lazyEvalGCall(name: String, t: Term, args: List[Term]) : Constructor = t match {

    case Constructor(cname, cargs) => {
      val gFunction = program.getGFunction(name, cname)
      val substitution: Map[Variable, Term] =
        Map() ++  ((gFunction.arg0.args zip cargs) ::: (gFunction.args zip args))      
      lazyEval(applySubstitution(gFunction.term, substitution))
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
    applySubstitution(fFunction.term, substitution)    
  }
  
  def illegalTerm(t: Term): Constructor = {
    throw new IllegalArgumentException(t + " is encoutered in passed expression. This term contains vars.")
  }
}