package spsc

import Algebra._
import Decomposition._

class BaseSuperCompiler(p: Program) {

  def driveExp(expr: Term): List[(Term, Contraction)] = decompose(expr) match {
    case DecLet(Let(term, bs)) => (term, null) :: bs.map { case (_, v) => (v, null) }
    case ObservableVar(v) => Nil
    case ObservableCtr(Ctr(_, args)) => args.map { (_, null) }
    case context@Context(red) =>
      red match {
        case RedexFCall(FCall(name, args)) => {
          val fReduced = subst(p.f(name).term, Map(p.f(name).args.zip(args): _*))
          List((context.replaceRedex(fReduced), null))
        }
        case RedexGCallCtr(GCall(name, args), Ctr(cname, cargs)) => {
          val g = p.g(name, cname)
          val gReduced = subst(g.term, Map((g.p.args ::: g.args) zip (cargs ::: args.tail): _*))
          List((context.replaceRedex(gReduced), null))
        }
        case RedexGCallVar(GCall(name, args), v) => {
          p.gs(name) map { g =>
            val fp = freshPat(g.p)
            val gReduced = subst(g.term, Map((g.p.args ::: g.args) zip (fp.args ::: args.tail): _*))
            (context.replaceRedex(gReduced), Contraction(v, fp))
          }
        }
      }
  }

  def buildProcessTree(e: Term): Tree = {
    var t = new Tree(new Node(e, null, null), Map().withDefaultValue(Nil))
    while (t.leaves.exists { !_.isProcessed }) {
      val b = t.leaves.find(!_.isProcessed).get
      t = b.ancestors.find(a => !trivial(a.expr) && inst(a.expr, b.expr)) match {
        case Some(a) => t.replace(b, Let(a.expr, findSubst(a.expr, b.expr).toList))
        case None => t.addChildren(b, driveExp(b.expr)) // drive
      }
    }
    t
  }

  def freshPat(p: Pat) = Pat(p.name, p.args map freshVar)
}