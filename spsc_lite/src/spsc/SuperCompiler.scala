package spsc
import Algebra._
class SuperCompiler(p: Program){
  def driveExp(expr: Term): List[(Term, Branch)] = expr match {
    case Cons(name, args) => args.map((_,null))
    case FCall(name, args)  => List((sub(p.f(name).term, Map(p.f(name).args.zip(args): _*)), null))
    case GCall(name, Cons(cname, cargs) :: args) =>
      val g = p.g(name, cname)  
      List((sub(g.term, Map((g.p.args:::g.args) zip (cargs ::: args): _*)), null))
    case gCall @ GCall(name, (v : Var) :: args) => 
      for (g <- p.gs(name); val pat = freshPat(g.p); val cons = Cons(pat.name, pat.args))
        yield (driveExp(sub(gCall, Map(v -> cons)))(0)._1, Branch(v, pat))
    case GCall(name, call :: args) => driveExp(call) map {p => (GCall(name, p._1 :: args), p._2)}
    case Let(term, bs) => (term, null) :: bs.map {pair => (pair._2, null)}
  }
  
  def buildProcessTree(e: Term): Tree = {
    val t = new Tree(new Node(e, null, Nil))
    while (!t.leafs.forall{_.isProcessed}) {
      val b = t.leafs.find(!_.isProcessed).get
      if (trivial(b.expr)) {
        t.addChildren(b, driveExp(b.expr))
      } else {
        b.ancestors.find(a => inst(a.expr, b.expr)) match {
          case Some(a) => if (equiv(a.expr, b.expr)) b.fnode = a else split(t, b, a)
          case None => t.addChildren(b, driveExp(b.expr))
        }
      }
    }   
    t
  }
  
  def split(t: Tree, a: Node, b: Node) = t.replace(a, Let(a.expr, findSub(a.expr, b.expr).toList))
  def trivial(expr: Term): Boolean = expr match {case x: Call => false; case _ => true}
  private var i = 0
  private def freshPat(p: Pattern) = Pattern(p.name, p.args.map {a => i += 1; Var("v" + i)})
}