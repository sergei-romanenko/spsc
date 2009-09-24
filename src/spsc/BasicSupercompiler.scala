package spsc

import Algebra._

class BasicSupercompiler(p: Program){
  def driveExp(expr: Term): List[(Term, Contraction)] = expr match {
    case Ctr(name, args) => args.map((_,null))
    case FCall(name, args)  =>
      List((applySubst(Map(p.f(name).args.zip(args): _*), p.f(name).term), null))
    case GCall(name, Ctr(cname, cargs) :: args) =>
      val g = p.g(name, cname)  
      List((applySubst(Map((g.p.args:::g.args) zip (cargs:::args): _*), g.term), null))
    case gCall @ GCall(name, (v : Var) :: args) => 
      for (g <- p.gs(name); fp = freshPat(g.p); ctr = Ctr(fp.name, fp.args)) 
        yield driveExp(applySubst(Map(v -> ctr), gCall)) match 
          {case (k, _) :: _ => (k, Contraction(v, fp))} 
    case GCall(name, args) => 
      driveExp(args(0)) map {case (k, v) => (GCall(name, k :: args.tail), v)}
    case Let(term, bs) => (term, null) :: bs.map {case (_, v) => (v, null)}
  }
 
  def buildProcessTree(e: Term): Tree = {
    var t = Tree.create(e)
    while (t.leaves.exists{!_.isProcessed}) {
      val b = t.leaves.find(!_.isProcessed).get
      t = b.ancestors.find(a => isFGCall(a.expr) && instOf(b.expr, a.expr)) match {
        case Some(a) => t.replace(b, Let(a.expr, matchAgainst(a.expr, b.expr).toList))
        case None => t.addChildren(b, driveExp(b.expr)) // drive
      }
    }
    t
  } 
 
  def freshPat(p: Pat) = Pat(p.name, p.args map freshVar)
}