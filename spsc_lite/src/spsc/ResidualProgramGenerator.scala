package spsc

import Util._
import scala.collection.jcl.LinkedHashSet

class ResidualProgramGenerator(val tree: ProcessTree) {
  import ResidualProgramGenerator._
  
  private var signatures = scala.collection.mutable.Map[Node, Signature]()
  private val defs = new scala.collection.mutable.ListBuffer[Definition]
  private val fnames = scala.collection.mutable.Set[String]()
  
  private val rootName = tree.root.expr.asInstanceOf[FCall].name
  
  private def generateProgram(): Program = {
    val t = unfold(tree.root)
    val rootCall = tree.root.expr.asInstanceOf[FCall]
    signatures.get(tree.root) match {
      case None => defs += FFunction(rootCall.name, rootCall.args.map(_.asInstanceOf[Variable]), t)
      case _ =>
    }    
    val newDefs = new scala.collection.mutable.ListBuffer[Definition]
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
      val sub = Util.sub(pGc, gc).get
      if (pNode.outs.size == 1){
        FCall(pSign.name, pSign.args.map(applySub(_, sub)))
      } else {
        GCall(pSign.name, applySub(pSign.args.head, sub) :: pSign.args.tail.map(applySub(_, sub)))
      }
    } else node.expr match {
      case v: Variable => v
      case Constructor(name, args) => Constructor(name, node.outs.map(e => unfold(e.child)))
    
      case LetExpression(term, bindings) => 
        applySub(unfold(node.outs.head.child), 
          Map() ++ (bindings.map(pair => pair._1) zip node.outs.tail.map(e => unfold(e.child))))
    
      case fc @ FCall(name, args) =>
        tree.leafs.find(_.repeated == node) match {
          case None => unfold(node.outs.head.child)
          case Some(fc1) => {
            val newName = if (node == tree.root) fc.name else rename(fc.name, node == tree.root)
            val signature = Signature(newName, getVars(fc).toList)
            signatures(node) = signature
            val result = unfold(node.outs.head.child)
            defs += FFunction(signature.name, signature.args, result)
            result
          }
        }
      
    
    case gc @ GCall(name, arg0 :: args) => 
      if (node.outs.head.substitution.isEmpty) {
        tree.leafs.find(_.repeated == node) match {
          case None => unfold(node.outs.head.child)
          case Some(fc1) => {
            val newName = if (node == tree.root) gc.name else rename(gc.name, node == tree.root)
            val signature = Signature(newName, getVars(gc).toList)
            signatures(node) = signature
            val result = unfold(node.outs.head.child)
            defs += FFunction(signature.name, signature.args, result)
            result
          }
        }
      } else {
        val patternVar = node.outs.head.substitution.toList.head._1
        val vars = (getVars(gc) - patternVar).toList
        val signature = Signature(rename(gc.name, node == tree.root), patternVar :: vars)
        signatures(node) = signature
        for (edge <- node.outs){
          val e = edge.substitution(patternVar).asInstanceOf[Constructor]
          val patName = e.name
          var patVars = e.args.map(_.asInstanceOf[Variable])
          defs += GFunction(signature.name, Pattern(patName, patVars), vars, unfold(edge.child))
        }
        GCall(signature.name, patternVar :: vars)
      }
  }
  
  private def getVars(t: Term): LinkedHashSet[Variable] = {
    val vars = new LinkedHashSet[Variable]()
    t match {
      case v: Variable => vars + v
      case c: Constructor => c.args.map(arg => {vars ++ getVars(arg)})
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
  case class Signature(name: String, args: List[Variable])
  def generateResidualProgram(tree: ProcessTree) = new ResidualProgramGenerator(tree).generateProgram()
  val letters = "abcdefghijklmnopqrstuvwxyz".toArray
  
  def getVar(n: Int): Variable = {
    val sb = new StringBuilder
    for (s <- Integer.toString(n, 26)) {
      sb.append(letters(Integer.parseInt("" + s, 26)))
    } 
    Variable(sb.toString)
  }
  
  def renameVarsInDefinition(d: Definition) = d match {
    case f: FFunction =>
      val args = f.args
      val renaming = Map() ++ ((args) zip (args.indices.map(getVar(_)))) 
      FFunction(f.name, f.args.map(renaming(_)), applySub(f.term, renaming))
    case g: GFunction =>
      val args = g.arg0.args ::: g.args
      val renaming = Map() ++ ((args) zip (args.indices.map(getVar(_))))
      GFunction(g.name, Pattern(g.arg0.name, g.arg0.args.map(renaming(_))), g.args.map(renaming(_)), applySub(g.term, renaming))    
  }

}
