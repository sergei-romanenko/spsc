package spsc
import Util._
class SuperCompiler(p: Program){
  def driveExp(expr: Term): List[(Term, (Var, Cons))] = expr match {
    case Cons(name, args) => args.map((_,null))
    case FCall(name, args)  => List((sub(p.f(name).term, Map(p.f(name).args zip args : _*)), null))
    case GCall(name, Cons(cname, cargs) :: args) => {
      val g = p.g(name, cname)  
      (sub(g.term, Map((g.p.args:::g.args).zip(cargs ::: args): _*)), null) :: Nil
    }
    case gCall @ GCall(name, (v : Var) :: args) => 
      for (g <- p.gs(name); val c = Cons(g.p.name, g.p.args.map(v => nextVar)))
        yield (driveExp(sub(gCall, Map(v -> c))).head._1, (v, c))
    case GCall(name, call :: args) => driveExp(call) map {p => (GCall(name, p._1 :: args), p._2)}
    case Let(term, bs) => (term, null) :: bs.map {pair => (pair._2, null)}
  }
  
  def buildProcessTree(e: Term): ProcessTree = {
    val t = new ProcessTree(new Node(e, null, Nil))
    while (!t.isClosed) {
      val b = t.leafs.find(!_.isProcessed).get
      if (trivial(b.expr)) {
        t.addChildren(b, driveExp(b.expr))
      } else {
        b.ancestors.find(a => inst(a.expr, b.expr)) match {
          case Some(a) => if (equiv(a.expr, b.expr)) b.repeated = a else split(t, b, a)
          case None => t.addChildren(b, driveExp(b.expr))
        }
      }
    }   
    t
  }
  
  def split(t: ProcessTree, alpha: Node, beta: Node): Unit =
    t.replace(alpha, Let(alpha.expr, findSub(alpha.expr, beta.expr).get.toList))
  
  def trivial(expr: Term): Boolean = expr match {
    case l: Let => !l.bindings.isEmpty
    case c: Cons => true
    case v: Var => true
    case _ => false
  }
  private var counter = 0
  def nextVar(): Var = {
    counter += 1
    Var("$" + counter)
  }
  
}