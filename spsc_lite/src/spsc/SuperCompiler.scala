package spsc;

import Util._

class SuperCompiler(program: Program){
  private var counter = 0
  def nextVar(): Variable = {
    counter += 1
    Variable("$" + counter)
  }
  
  def driveExp(expr: Term): List[Pair[Term, Map[Variable, Term]]] = expr match {
    case v: Variable => Nil
    case Constructor(name, args) => 
      args.map((_, Map()))
    case FCall(name, args)  => {
      val fDef = program.f(name)
      List((applySub(fDef.term, Map(fDef.args zip args : _*)), Map()))
    }
    case GCall(name, Constructor(cname, cargs) :: args) => {
      val gDef = program.g(name, cname)  
      List((applySub(gDef.term, Map(((gDef.arg0.args zip cargs) ::: (gDef.args zip args)) : _*)), Map()))
    }
    case gCall @ GCall(name, (v : Variable) :: args) => 
      for (g <- program.gs(name);
        val c = Constructor(g.arg0.name, g.arg0.args.map(v => nextVar));
        val sub = Map(v -> c))
        yield (driveExp(applySub(gCall, sub)).head._1, sub)
    case GCall(name, call :: args) =>
      driveExp(call).map(p => (GCall(name, p._1 :: args.map(applySub(_, p._2)) ), p._2))
    case LetExpression(term, bindings) => 
      (term, Map[Variable, Term]()) :: (for (pair <- bindings) yield Pair(pair._2, Map[Variable, Term]())).toList
  }
  
  // heart of supercompiler
  def buildProcessTree(e: Term): ProcessTree = {
    val p = new ProcessTree(new Node(e, null, Nil))
    while (!p.isClosed) {
      val beta = p.leafs.find(!_.isProcessed).get
      if (isTrivial(beta.expr)) {
        p.addChildren(beta, driveExp(beta.expr))
      } else {
        beta.ancestors.find(n1 => !isTrivial(n1.expr) && equiv(n1.expr, beta.expr)) match {
          case Some(alpha) => beta.repeated = alpha
          case None => beta.ancestors.find(n1 => !isTrivial(n1.expr) && inst(n1.expr, beta.expr)) match {
            case Some(alpha) => makeAbstraction(p, beta, alpha)
            case None => p.addChildren(beta, driveExp(beta.expr))
          }
        }
      }
    }    
    p
  }
  
  def makeAbstraction(t: ProcessTree, alpha: Node, beta: Node): Unit = {
    val g = sub(alpha.expr.asInstanceOf[Term], beta.expr.asInstanceOf[Term]).get
    t.replace(alpha, LetExpression(alpha.expr.asInstanceOf[Term], (Map() ++ g).toList))
  }
  
  def isTrivial(expr: Term): Boolean = expr match {
    case l: LetExpression => !l.bindings.isEmpty
    case c: Constructor => true
    case v: Variable => true
    case _ => false
  }
  
}