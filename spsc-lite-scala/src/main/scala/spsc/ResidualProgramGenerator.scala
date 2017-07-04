package spsc
import Algebra._

class ResidualProgramGenerator(val tree: Tree) {
  
  private val sigs = scala.collection.mutable.Map[Node, (String, List[Var])]()
  private val defs = new scala.collection.mutable.ListBuffer[Rule]
  lazy val result = (walk(tree.root), Program(defs.toList))
  
  private def walk(n: Node): Term = if (n.funcAncestor == null) n.expr match {
    case v: Var => v
    case Let(_,bs) =>
      var body = walk(tree.children(n).head)
      applySubst(Map(bs map {case (k, _) => k} zip (tree.children(n).tail map walk):_*),
                 body)
    case Ctr(name, _) => Ctr(name, tree.children(n).map(walk))
    case FCall(name, args) => walkCall(n, name, args)
    case GCall(name, args) => walkCall(n, name, args)
  } else sigs(n.funcAncestor) match {
    case (name, args) => 
      if (tree.children(n.funcAncestor).head.contr == null) 
           applySubst(matchAgainst(n.funcAncestor.expr, n.expr), FCall(name, args))
      else applySubst(matchAgainst(n.funcAncestor.expr, n.expr), GCall(name, args))
  }

  def walkCall(n: Node, name: String, args: List[Term]) = {
    val vs = vars(n.expr)
    if (tree.children(n).head.contr != null) {
      val (gname, _) = sigs.getOrElseUpdate(n, (rename(name, "g"), vs))
      for (cn <- tree.children(n)) 
        defs += GRule(gname, cn.contr.pat, vs.tail, walk(cn))
      GCall(gname, vs)
    } else if (tree.leaves.exists(_.funcAncestor == n)) {
      val (fname, fargs) = sigs.getOrElseUpdate(n, (rename(name, "f"), vs))
      defs += FRule(fname, fargs, walk(tree.children(n).head))
      FCall(fname, vs)
    } else walk(tree.children(n).head)
  }
  
  def rename(f: String, b: String) = {b + f.drop(1) + (sigs.size + 1)}
}