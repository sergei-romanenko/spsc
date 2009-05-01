package spsc
import Algebra._
class ResidualProgramGenerator(val tree: Tree1) {
  lazy val residualProgram: Program = {
    val t = walk(tree.root)
    val rootCall = tree.root.expr.asInstanceOf[FCall]
    if (sigs.get(tree.root).isEmpty) defs += FFun(rootCall.name, vars(rootCall), t)
    Program(defs.toList)
  }
  
  private def walk(n: Node1): Term = if (n.fnode == null) n.expr match {
    case v: Var => v
    case Ctr(name,args) => Ctr(name, tree.children(n).map(walk))
    case Let(_,bs) => sub(walk(tree.children(n)(0)), Map(bs.map{_._1}.zip(tree.children(n).tail.map(walk)):_*))
    case call: Call =>
      if (tree.children(n)(0).branch != null) {
        sigs(n) = (rename(call.name, false, "g"), vars(call))
        for (cn <- tree.children(n)) 
          defs += GFun(sigs(n)._1, cn.branch.pat, vars(call).tail, walk(cn))
        GCall(sigs(n)._1, vars(call))
      } else if (tree.leafs.exists(_.fnode == n)) {
        sigs(n) = (rename(call.name, n == tree.root, "f"), vars(call))
        defs += FFun(sigs(n)._1, sigs(n)._2, walk(tree.children(n)(0)))
        FCall(sigs(n)._1, vars(call))
      } else walk(tree.children(n)(0))
  } else if (tree.children(n.fnode)(0).branch == null)
    sub(FCall(sigs(n.fnode)._1, sigs(n.fnode)._2), findSub(n.fnode.expr, n.expr))
  else
    sub(GCall(sigs(n.fnode)._1, sigs(n.fnode)._2), findSub(n.fnode.expr, n.expr))
  
  private var sigs = scala.collection.mutable.Map[Node1, (String, List[Var])]()
  private val defs = new scala.collection.mutable.ListBuffer[Def]
  var i = 0
  def rename(f: String, keep: Boolean, b: String) = if (keep) f else {i+=1; b + f.drop(1) + i}
}