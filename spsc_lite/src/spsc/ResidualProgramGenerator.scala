package spsc
import Util._

class ResidualProgramGenerator(val tree: Tree) {
  private def walk(n: Node): Term = if (n.fnode == null) n.expr match {
    case v: Var => v
    case Cons(name,args) => Cons(name, n.children.map(walk))
    case Let(_,bs) => sub(walk(n.children(0)), Map(bs.map{_._1}.zip(n.children.tail.map(walk)):_*))
    case call: Call =>
      if (n.outs(0).branch != null) {
        sigs(n) = (rename(call.f, false, false), vars(call))
        for (e <- n.outs) defs += GFun(sigs(n)._1, e.branch.pat, vars(call).tail, walk(e.child))
        GCall(sigs(n)._1, vars(call))
      } else if (tree.leafs.exists(_.fnode == n)) {
        sigs(n) = (rename(call.f, n == tree.root, true), vars(call))
        defs += FFun(sigs(n)._1, sigs(n)._2, walk(n.children(0)))
        FCall(sigs(n)._1, vars(call))
      } else walk(n.children(0))
  } else if (n.fnode.outs(0).branch == null)
    sub(FCall(sigs(n.fnode)._1, sigs(n.fnode)._2), findSub(n.fnode.expr, n.expr))
  else
    sub(GCall(sigs(n.fnode)._1, sigs(n.fnode)._2), findSub(n.fnode.expr, n.expr))
  
  private var sigs = scala.collection.mutable.Map[Node, (String, List[Var])]()
  private val defs = new scala.collection.mutable.ListBuffer[Def]
  
  def generateProgram(): Program = {
    val t = walk(tree.root)
    val rootCall = tree.root.expr.asInstanceOf[FCall]
    if (sigs.get(tree.root).isEmpty) defs += FFun(rootCall.name, vars(rootCall), t)
    Program(defs.toList)
  }
  
  var i=0
  private def rename(name: String, keep: Boolean, isF: Boolean) = 
    if (keep) name else { i+=1; if (isF) "f" +name.drop(1) + i else "g" +name.drop(1) + i}
}