package spsc

import Algebra._
class SuperCompiler0(p: Program){
  def driveExp(expr: Term): List[(Term, Contraction)] = expr match {
    case Ctr(name, args) => args.map((_,null))
    case FCall(name, args)  =>
      List((subst(p.f(name).term, Map(p.f(name).args.zip(args): _*)), null))
    case GCall(name, Ctr(cname, cargs) :: args) =>
      val g = p.g(name, cname)  
      List((subst(g.term, Map((g.p.args:::g.args) zip (cargs:::args): _*)), null))
    case gCall @ GCall(name, (v : Var) :: args) => 
      for (g <- p.gs(name); fp = freshPat(g.p); cons = Ctr(fp.name, fp.args))
        yield (driveExp(subst(gCall, Map(v -> cons)))(0)._1, Contraction(v, fp))
    case GCall(name, args) => 
      driveExp(args(0)) map {p => (GCall(name, p._1 :: args.tail), p._2)}
    case Let(term, bs) =>
      (term, null) :: bs.map {pair => (pair._2, null)}
  }
 
  def buildProcessTree(e: Term): Tree = {
    val n = new Node(e, null, null)
    var t = new Tree(n, Map().withDefaultValue(Nil))
    while (t.leaves.exists{!_.isProcessed}) {
      val b = t.leaves.find(!_.isProcessed).get
      t = b.ancestors.find(a => !trivial(a.expr) && inst(a.expr, b.expr)) match {
        case Some(a) => t.replace(b, Let(b.expr, findSubst(b.expr, a.expr).toList))
        case None => t.addChildren(b, driveExp(b.expr)) // drive
      }
    }
    t
  } 
 
  private def freshPat(p: Pat) = Pat(p.name, p.args map freshVar)
}