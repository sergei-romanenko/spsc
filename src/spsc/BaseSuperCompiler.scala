package spsc

import Algebra._
class BaseSuperCompiler(p: Program){
  def driveExp(expr: Term): List[(Term, Contraction)] = expr match {
    case Ctr(name, args) => args.map((_,null))
    case FCall(name, args)  =>
      List((subst(p.f(name).term, Map(p.f(name).args.zip(args): _*)), null))
    case GCall(name, Ctr(cname, cargs) :: args) =>
      val g = p.g(name, cname)  
      List((subst(g.term, Map((g.p.args:::g.args) zip (cargs:::args): _*)), null))
    case gCall @ GCall(name, (v : Var) :: args) => 
      for (g <- p.gs(name); fp = freshPat(g.p); cons = Ctr(fp.name, fp.args)) 
        yield driveExp(subst(gCall, Map(v -> cons))) match 
          {case (k, _) :: _ => (k, Contraction(v, fp))} 
    case GCall(name, args) => 
      driveExp(args(0)) map {case (k, v) => (GCall(name, k :: args.tail), v)}
    case Let(term, bs) => (term, null) :: bs.map {case (_, v) => (v, null)}
  }
 
  def buildProcessTree(e: Term): Tree = {
    var t = new Tree(new Node(e, null, null), Map().withDefaultValue(Nil))
    while (t.leaves.exists{!_.isProcessed}) {
      val b = t.leaves.find(!_.isProcessed).get
      t = b.ancestors.find(a => !trivial(a.expr) && inst(a.expr, b.expr)) match {
        case Some(a) => t.replace(b, Let(b.expr, findSubst(b.expr, a.expr).toList))
        case None => t.addChildren(b, driveExp(b.expr))
      }
    }
    t
  } 
 
  def freshPat(p: Pat) = Pat(p.name, p.args map freshVar)
}