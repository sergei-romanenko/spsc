package spsc
import Algebra._
class ResidualProgramGenerator(val tree: Tree) {
  private val sigs = scala.collection.mutable.Map[Node, (String, List[Var])]()
  private val defs = new scala.collection.mutable.ListBuffer[Def]
  lazy val result = (walk(tree.root), Program(defs.toList))
  
  private def walk(n: Node): Term = if (n.fnode == null) n.expr match {
    case v: Var => v
    case Ctr(name,args) => Ctr(name, tree.children(n).map(walk))
    case Let(_,bs) => sub(walk(tree.children(n)(0)), Map(bs.map{_._1}.zip(tree.children(n).tail.map(walk)):_*))
    case call: Call =>
      if (tree.children(n)(0).branch != null) {
        sigs(n) = (rename(call.name, "g"), vars(call))
        for (cn <- tree.children(n)) 
          defs += GFun(sigs(n)._1, cn.branch.pat, vars(call).tail, walk(cn))
        GCall(sigs(n)._1, vars(call))
      } else if (tree.leaves.exists(_.fnode == n)) {
        sigs(n) = (rename(call.name, "f"), vars(call))
        defs += FFun(sigs(n)._1, sigs(n)._2, walk(tree.children(n)(0)))
        FCall(sigs(n)._1, vars(call))
      } else walk(tree.children(n)(0))
  } else if (tree.children(n.fnode)(0).branch == null)
    sub(FCall(sigs(n.fnode)._1, sigs(n.fnode)._2), findSub(n.fnode.expr, n.expr))
  else
    sub(GCall(sigs(n.fnode)._1, sigs(n.fnode)._2), findSub(n.fnode.expr, n.expr))

  def rename(f: String, b: String) = {b + f.drop(1) + sigs.size}
}