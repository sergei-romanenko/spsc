package spsc;

import Util._

class SuperCompiler(program: Program){
  private var counter = 0
  def nextVar(): Var = {
    counter += 1
    Var("$" + counter)
  }
  
  def driveExp(expr: Term): List[Pair[Term, Map[Var, Term]]] = expr match {
    case v: Var => Nil
    case Cons(name, args) => args.map((_, Map()))
    case FCall(name, args)  => {
      val fDef = program.f(name)
      List((sub(fDef.term, Map(fDef.args zip args : _*)), Map()))
    }
    case GCall(name, Cons(cname, cargs) :: args) => {
      val gDef = program.g(name, cname)  
      List((sub(gDef.term, Map(((gDef.p.args zip cargs) ::: (gDef.args zip args)) : _*)), Map()))
    }
    case gCall @ GCall(name, (v : Var) :: args) => 
      for (g <- program.gs(name);
        val c = Cons(g.p.name, g.p.args.map(v => nextVar));
        val s = Map(v -> c))
        yield (driveExp(sub(gCall, s)).head._1, s)
    case GCall(name, call :: args) =>
      driveExp(call).map(p => (GCall(name, p._1 :: args.map(sub(_, p._2)) ), p._2))
    case Let(term, bs) => 
      (term, Map[Var, Term]()) :: (for (pair <- bs) yield Pair(pair._2, Map[Var, Term]())).toList
  }
  
  def buildProcessTree(e: Term): ProcessTree = {
    val p = new ProcessTree(new Node(e, null, Nil))
    while (!p.isClosed) {
      val b = p.leafs.find(!_.isProcessed).get
      if (trivial(b.expr)) {
        p.addChildren(b, driveExp(b.expr))
      } else {
        b.ancestors.find(a => equiv(a.expr, b.expr)) match {
          case Some(a) => b.repeated = a
          case None => b.ancestors.find(a => !trivial(a.expr) && inst(a.expr, b.expr)) match {
            case Some(alpha) => makeAbstraction(p, b, alpha)
            case None => p.addChildren(b, driveExp(b.expr))
          }
        }
      }
    }    
    p
  }
  
  def makeAbstraction(t: ProcessTree, alpha: Node, beta: Node): Unit = {
    val g = findSub(alpha.expr.asInstanceOf[Term], beta.expr.asInstanceOf[Term]).get
    t.replace(alpha, Let(alpha.expr.asInstanceOf[Term], (Map() ++ g).toList))
  }
  
  def trivial(expr: Term): Boolean = expr match {
    case l: Let => !l.bindings.isEmpty
    case c: Cons => true
    case v: Var => true
    case _ => false
  }
  
}