package spsc
import Util._
import scala.collection.jcl.LinkedHashSet
class ResidualProgramGenerator(val tree: Tree) {
  import ResidualProgramGenerator._
  private def walk(n: Node): Term = if (n.fnode == null) n.expr match {
    case v: Var => v
    case Cons(name, args) => Cons(name, n.children.map(walk))
    case Let(_, bs) => sub(walk(n.children(0)), Map(bs.map{_._1}.zip(n.children.tail.map(walk)):_*))
    case call: Call =>
      if (n.outs(0).branch != null) {
        val patternVar = n.outs(0).branch.v
        val vars = (getVars(call) - patternVar).toList
        sigs(n) = Signature(rename(call.f, false), patternVar :: vars)
        for (e <- n.outs) defs += GFun(sigs(n).name, e.branch.pat, vars, walk(e.child))
        GCall(sigs(n).name, patternVar :: vars)
      } else if (tree.leafs.exists(_.fnode == n)) {
        sigs(n) = Signature(rename(call.f, n == tree.root), getVars(call).toList)
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
  private val fnames = scala.collection.mutable.Set[String]()
  private val rootName = tree.root.expr.asInstanceOf[FCall].name
  
  private def generateProgram(): Program = {
    val t = walk(tree.root)
    val rootCall = tree.root.expr.asInstanceOf[FCall]
    sigs.get(tree.root) match {
      case None => defs += FFun(rootCall.name, rootCall.args.map(_.asInstanceOf[Var]), t)
      case _ =>
    }
    Program(defs.toList)
  }
  
  private def getVars(t: Term): List[Var] = t match {
      case v: Var   => (List(v))
      case c: Cons  => (List[Var]() /: c.args) {case (l, a) => l union getVars(a)}
      case f: FCall => (List[Var]() /: f.args) {case (l, a) => l union getVars(a)}
      case g: GCall => (List[Var]() /: g.args) {case (l, a) => l union getVars(a)}
    }
  
  private def rename(name: String, isOriginalNameAllowed: Boolean) = {
    if (isOriginalNameAllowed && !fnames.contains(name)){
      fnames += name
      name
    } else {
      var index = 1
      var newName = name + index
      while(rootName==newName || fnames.contains(newName)){
        index += 1
        newName = name + index
      }
      fnames += newName
      newName
    }
  }
}

object ResidualProgramGenerator{
  case class Signature(name: String, args: List[Var])
  def generateResidualProgram(tree: Tree) = new ResidualProgramGenerator(tree).generateProgram()
}
