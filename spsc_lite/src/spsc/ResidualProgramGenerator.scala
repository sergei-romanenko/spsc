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
  } else if (n.fnode.outs.size == 1)
    sub(FCall(sigs(n.fnode).name, sigs(n.fnode).args), Util.findSub(n.fnode.expr, n.expr))
  else
    sub(GCall(sigs(n.fnode).name, sigs(n.fnode).args), Util.findSub(n.fnode.expr, n.expr))
  
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
    val newDefs = new scala.collection.mutable.ListBuffer[Def]
    for (d <- defs) {
      newDefs += renameVarsInDefinition(d)
    }
    Program(newDefs.toList.sort((e1, e2) => (e1.name compareTo e2.name) < 0))
  }
  
  private def getVars(t: Term): LinkedHashSet[Var] = {
    val vars = new LinkedHashSet[Var]()
    t match {
      case v: Var => vars + v
      case c: Cons => c.args.map(arg => {vars ++ getVars(arg)})
      case f: FCall => f.args.map(arg => {vars ++ getVars(arg)})
      case g: GCall => (g.args).map(arg => {vars ++ getVars(arg)})
    }
    vars
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
  val letters = "abcdefghijklmnopqrstuvwxyz".toArray
  def getVar(n: Int): Var = Var(new String(letters.slice(n, n+1)))
  def renameVarsInDefinition(d: Def) = d match {
    case f: FFun =>
      val args = f.args
      val renaming = Map() ++ ((args) zip (args.indices.map(getVar(_)))) 
      FFun(f.name, f.args.map(renaming(_)), sub(f.term, renaming))
    case g: GFun =>
      val args = g.p.args ::: g.args
      val renaming = Map() ++ ((args) zip (args.indices.map(getVar(_))))
      GFun(g.name, Pattern(g.p.name, g.p.args.map(renaming(_))), g.args.map(renaming(_)), sub(g.term, renaming))    
  }
}
