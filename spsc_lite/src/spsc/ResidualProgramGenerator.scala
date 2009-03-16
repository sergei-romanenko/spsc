package spsc
import Util._
case class Signature(name: String, args: List[Var])
class ResidualProgramGenerator(val tree: Tree) {
  private def walk(n: Node): Term = if (n.fnode == null) n.expr match {
    case v: Var => v
    case Cons(name, args) => Cons(name, n.children.map(walk))
    case Let(_, bs) => sub(walk(n.children(0)), Map(bs.map{_._1}.zip(n.children.tail.map(walk)):_*))
    case call: Call =>
      if (n.outs(0).branch != null) {
        val patternVar = n.outs(0).branch.v
        val vars = getVars(call) - patternVar
        sigs(n) = Signature(rename(call.f, false, false), patternVar :: vars)
        for (e <- n.outs) defs += GFun(sigs(n).name, e.branch.pat, vars, walk(e.child))
        GCall(sigs(n).name, patternVar :: vars)
      } else if (tree.leafs.exists(_.fnode == n)) {
        sigs(n) = Signature(rename(call.f, n == tree.root, true), getVars(call))
        val body = walk(n.children(0))
        defs += FFun(sigs(n).name, sigs(n).args, body)
        body
      } else walk(n.children(0))
  } else if (n.fnode.outs(0).branch == null)
    sub(FCall(sigs(n.fnode).name, sigs(n.fnode).args), findSub(n.fnode.expr, n.expr))
  else
    sub(GCall(sigs(n.fnode).name, sigs(n.fnode).args), findSub(n.fnode.expr, n.expr))
  
  private var sigs = scala.collection.mutable.Map[Node, Signature]()
  private val defs = new scala.collection.mutable.ListBuffer[Def]
  
  def generateProgram(): Program = {
    val t = walk(tree.root)
    val rootCall = tree.root.expr.asInstanceOf[FCall]
    sigs.get(tree.root) match {
      case None => defs += FFun(rootCall.name, rootCall.args.map(_.asInstanceOf[Var]), t)
      case _ =>
    }
    Program(defs.toList)
  }
  
  var i=0
  private def rename(name: String, keep: Boolean, isF: Boolean) = 
    if (keep) name else { i+=1; if (isF) "f" +name.drop(1) + i else "g" +name.drop(1) + i}
}