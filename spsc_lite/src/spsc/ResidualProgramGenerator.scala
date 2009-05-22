package spsc
import Algebra._

class ResidualProgramGenerator(val tree: Tree) {
  
  private val sigs = scala.collection.mutable.Map[Node, (String, List[Var])]()
  private val defs = new scala.collection.mutable.ListBuffer[Def]
  lazy val result = (walk(tree.root), Program(defs.toList))
  
  private def walk(n: Node): Term = if (n.fnode == null) n.expr match {
    case v: Var => v
    case Let(_,bs) => subst(walk(tree.children(n)(0)), 
                       Map(bs.map{_._1}.zip(tree.children(n).tail.map(walk)):_*))
    case Ctr(name, _) => Ctr(name, tree.children(n).map(walk))
    case FCall(name, args) => walkCall(n, name, args)
    case GCall(name, args) => walkCall(n, name, args)
  } else sigs(n.fnode) match {
    case (name, args) => if (tree.children(n.fnode)(0).contr == null) 
           subst(FCall(name, args), findSubst(n.fnode.expr, n.expr))
      else subst(FCall(name, args), findSubst(n.fnode.expr, n.expr))
  }

  def walkCall(n: Node, name: String, args: List[Term]): Term = {
    val vs = vars(n.expr)
    if (tree.children(n)(0).contr != null) {
      sigs(n) = (rename(name, "g"), vs)
      for (cn <- tree.children(n)) 
        defs += GFun(sigs(n)._1, cn.contr.pat, vs.tail, walk(cn))
      GCall(sigs(n)._1, vs)
    } else if (tree.leaves.exists(_.fnode == n)) {
      sigs(n) = (rename(name, "f"), vs)
      defs += FFun(sigs(n)._1, sigs(n)._2, walk(tree.children(n)(0)))
      FCall(sigs(n)._1, vs)
    } else walk(tree.children(n)(0))
  }
  
  def rename(f: String, b: String) = {b + f.drop(1) + sigs.size}
}