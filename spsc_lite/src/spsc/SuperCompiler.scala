package spsc;

import SmallLanguageTermAlgebra._
import Util.applySub

class SuperCompiler(program: Program){
  def driveExp(expr: Expression): List[Pair[Term, Map[Variable, Term]]] = expr match {
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
  def buildProcessTree(e: Expression): ProcessTree = {
    val p = new ProcessTree(new Node(e, null, Nil))
    while (!p.isClosed) {
      val beta = p.leafs.find(!_.isProcessed).get
      if (isTrivial(beta.expr)) {
        drive(p, beta)
      } else {
        beta.ancestors.find(n1 => !isTrivial(n1.expr) && equiv(n1.expr.asInstanceOf[Term], beta.expr.asInstanceOf[Term])) match {
          case Some(alpha) => beta.repeated = alpha
          case None => beta.ancestors.find(n1 => !isTrivial(n1.expr) && instanceOf(n1.expr.asInstanceOf[Term], beta.expr.asInstanceOf[Term])) match {
            case Some(alpha) => makeAbstraction(p, beta, alpha)
            case None => drive(p, beta)
          }
        }
      }
    }    
    p
  }
  
  def drive(t: ProcessTree, n: Node): Unit = {
    t.addChildren(n, driveExp(n.expr))
    println()
  }
  
  def makeAbstraction(t: ProcessTree, alpha: Node, beta: Node): Unit = {
    val g = strongMsg(alpha.expr.asInstanceOf[Term], beta.expr.asInstanceOf[Term])
    if (g.sub1.isEmpty){
      t.replace(alpha, g.term)
    } else {
      t.replace(alpha, LetExpression(g.term, (Map() ++ g.sub1).toList))
    }
  }
  
  def equiv(term1: Term, term2: Term): Boolean = {
    var map1to2 = scala.collection.mutable.Map[Variable, Variable]()
    var map2to1 = scala.collection.mutable.Map[Variable, Variable]()
    def eq1(t1: Term, t2: Term): Boolean = (t1, t2) match {
      case (v1: Variable, v2: Variable) => (map1to2.get(v1), map2to1.get(v2)) match {
        case (Some(v3), Some(v4)) => v2 == v3 && v1 == v4
        case (None, None) => map1to2(v1) = v2; map2to1(v2) = v1; true
        case _ => false
      }
      case (t1, t2) => t1.productPrefix == t2.productPrefix && 
        t1.name == t2.name && ((t1.args zip t2.args) forall {case (a, b) => eq1(a, b)})
    }
    eq1(term1, term2)
  }
  
}