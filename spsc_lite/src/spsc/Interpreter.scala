package spsc

import scala.util.parsing.input.CharArrayReader

import Util._

// The operational semantics of the interpreter is
// the normal-order graph reduction to the weak head normal form.
class Interpreter (program: Program) {
  
  def this(text: String) = this(Util.programFromString(text))
  
  def eval(t: Term): Term = {
    val lazyResult = lazyEval(t) 
    Constructor(lazyResult.name, lazyResult.args.map(eval))
  }
  
  def eval(input: String): Term = {
    val pr = SmallLanguageParsers.parseTerm(new CharArrayReader(input.toCharArray))
    if (pr.isEmpty) throw new IllegalArgumentException(pr.toString)
    eval(correctCalls(pr.get, program))
  }
  
  private def lazyEval(t: Term): Constructor = t match {

    case c: Constructor => c
    
    case FCall(name, args)  => 
      lazyEval(unfoldFCall(name, args))

    case GCall(name, arg0 :: args) =>
      lazyEvalGCall(name, arg0, args)

    case t: Term =>
      illegalTerm(t)
  }

  def lazyEvalGCall(name: String, t: Term, args: List[Term]) : Constructor = t match {

    case Constructor(cname, cargs) => {
      val gFunction = program.g(name, cname)
      val substitution: Map[Variable, Term] =
        Map() ++  ((gFunction.arg0.args zip cargs) ::: (gFunction.args zip args))      
      lazyEval(applySub(gFunction.term, substitution))
    }
    
    case FCall(name1, args1) => 
      lazyEvalGCall(name, unfoldFCall(name1, args1), args)

    case GCall(name1, t1 :: args1) =>
      lazyEvalGCall(name, lazyEvalGCall(name1, t1, args1), args)

    case t : Term =>
      illegalTerm(t)
  }
  
  def unfoldFCall(name: String, args: List[Term]): Term = {
    val fFunction = program.f(name)
    val substitution: Map[Variable, Term] = Map() ++  (fFunction.args zip args)
    applySub(fFunction.term, substitution)    
  }
  
  def illegalTerm(t: Term): Constructor = {
    throw new IllegalArgumentException(t + " is encoutered in passed expression. This term contains vars.")
  }
}