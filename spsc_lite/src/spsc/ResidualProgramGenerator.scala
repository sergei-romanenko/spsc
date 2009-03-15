package spsc

import Util._
import scala.collection.jcl.LinkedHashSet

class ResidualProgramGenerator(val tree: ProcessTree) {
  import ResidualProgramGenerator._
  
  private var signatures = scala.collection.mutable.Map[Node, Signature]()
  private val defs = new scala.collection.mutable.ListBuffer[Def]
  private val fnames = scala.collection.mutable.Set[String]()
  
  private val rootName = tree.root.expr.asInstanceOf[FCall].name
  
  private def generateProgram(): Program = {
    val t = unfold(tree.root)
    val rootCall = tree.root.expr.asInstanceOf[FCall]
    signatures.get(tree.root) match {
      case None => defs += FFun(rootCall.name, rootCall.args.map(_.asInstanceOf[Var]), t)
      case _ =>
    }    
    val newDefs = new scala.collection.mutable.ListBuffer[Def]
    for (d <- defs) {
      newDefs += renameVarsInDefinition(d)
    }
    Program(newDefs.toList.sort((e1, e2) => (e1.name compareTo e2.name) < 0))
  }
  
  private def unfold(node: Node): Term = 
    if (node.repeated != null) {
      val gc = node.expr.asInstanceOf[Term]
      val pNode = node.repeated
      val pGc = pNode.expr.asInstanceOf[Term]
      val pSign = signatures(pNode)
      val s = Util.findSub(pGc, gc).get
      if (pNode.outs.size == 1){
        FCall(pSign.name, pSign.args.map(sub(_, s)))
      } else {
        GCall(pSign.name, pSign.args.map(sub(_, s)))
      }
    } else node.expr match {
      case v: Var => v
      case Cons(name, args) => Cons(name, node.outs.map(e => unfold(e.child)))
    
      case Let(term, bindings) => 
        sub(unfold(node.outs.head.child), 
          Map() ++ (bindings.map(pair => pair._1) zip node.outs.tail.map(e => unfold(e.child))))
      
      case call : Call => 
        if (node.outs.head.substitution == null) {
          tree.leafs.find(_.repeated == node) match {
            case None => unfold(node.outs.head.child)
            case Some(fc1) => {
              val newName = if (node == tree.root) call.f else rename(call.f, node == tree.root)
              val signature = Signature(newName, getVars(call).toList)
              signatures(node) = signature
              val result = unfold(node.outs.head.child)
              defs += FFun(signature.name, signature.args, result)
              result
            }
          }
        } else {
          val patternVar = node.outs.head.substitution._1
          val vars = (getVars(call) - patternVar).toList
          val signature = Signature(rename(call.f, node == tree.root), patternVar :: vars)
          signatures(node) = signature
          for (edge <- node.outs){
            val e = edge.substitution._2.asInstanceOf[Cons]
            val patName = e.name
            var patVars = e.args.map(_.asInstanceOf[Var])
            defs += GFun(signature.name, Pattern(patName, patVars), vars, unfold(edge.child))
          }
          GCall(signature.name, patternVar :: vars)
      }
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
  def generateResidualProgram(tree: ProcessTree) = new ResidualProgramGenerator(tree).generateProgram()
  val letters = "abcdefghijklmnopqrstuvwxyz".toArray
  
  def getVar(n: Int): Var = {
    val sb = new StringBuilder
    for (s <- Integer.toString(n, 26)) {
      sb.append(letters(Integer.parseInt("" + s, 26)))
    } 
    Var(sb.toString)
  }
  
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
