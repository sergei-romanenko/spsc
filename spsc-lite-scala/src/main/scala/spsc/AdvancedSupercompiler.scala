package spsc

import Algebra._

class AdvancedSupercompiler(p: Program) extends BasicSupercompiler(p){
  
  override def buildProcessTree(e: Term): Tree = {
    var t = Tree.create(e)
    while (t.leaves.exists{!_.isProcessed}) {
      val b = t.leaves.find(!_.isProcessed).get
      t = if (!isFGCall(b.expr)) {
        t.addChildren(b, driveExp(b.expr)) // drive
      } else {
        b.ancestors.find(a => isFGCall(a.expr) && HE.embeddedIn(a.expr, b.expr)) match {
          case Some(a) => {  
            if (instOf(b.expr, a.expr)) abs(t, b, a)
            else if (equiv(MSG.msg(a.expr, b.expr).t, Var("z"))) split(t, b)
            else abs(t, a, b)
          }
          case None => t.addChildren(b, driveExp(b.expr)) // drive
        }}}
    t
  }
  
  def abs(t: Tree, a: Node, b: Node) =
    ((g: Gen) => t.replace(a, Let(g.t, g.m1.toList))) (MSG.msg(a.expr, b.expr))
  
  def split(t: Tree, n: Node) : Tree = n.expr match {
    case e : CFG =>
      val vs = e.args map freshVar
      t.replace(n, Let(e.replaceArgs(vs), vs zip e.args))
    }
}