package spsc;

import SmallLanguage._
import ProcessTree._
import SmallLanguageTermAlgebra._
import Util.applySubstitution

class ResidualProgramGenerator(val tree: ProcessTree) {
  import ResidualProgramGenerator._
  
  private var signatures = scala.collection.mutable.Map[Node, Signature]()
  private val defs = new scala.collection.mutable.ListBuffer[Definition]
  private val fnames = scala.collection.mutable.Set[String]()
  
  private val rootName = tree.rootNode.expr.asInstanceOf[FCall].name
  
  private def generateProgram(): Program = {
    val t = unfold(tree.rootNode)
    val rootCall = tree.rootNode.expr.asInstanceOf[FCall]
    signatures.get(tree.rootNode) match {
      case None => defs += FFunction(rootCall.name, rootCall.args.map(_.asInstanceOf[Variable]), t)
      case _ =>
    }    
    val newDefs = new scala.collection.mutable.ListBuffer[Definition]
    for (d <- defs) {
      newDefs += renameVarsInDefinition(d)
    }
    Program(newDefs.toList.sort((e1, e2) => (e1.name compareTo e2.name) < 0))
  }
  
  private def unfold(node: Node): Term = node.expr match {
    case v: Variable => v
    
    case Constructor(name, args) => Constructor(name, node.outs.map(e => unfold(e.child)))
    
    case LetExpression(term, bindings) => 
      applySubstitution(unfold(node.outs.head.child), 
          Map() ++ (bindings.map(pair => pair._1) zip node.outs.tail.map(e => unfold(e.child))))
    
    case fc @ FCall(name, args) =>
      if (node.outs.isEmpty){
        val pNode = node.ancestors.find(n => n.expr match {case fc_ : FCall => equivalent(fc, fc_); case _=>false}).get
        val pFc = pNode.expr.asInstanceOf[FCall]
        val pSign = signatures(pNode)
        val sub = Map() ++ renaming(pFc, fc)
        FCall(pSign.name, pSign.args.map(applySubstitution(_, sub)))
      } else {
        tree.leafs.find(n => n.ancestors.contains(node) && 
        (n.expr match {case fc_ : FCall => equivalent(fc, fc_); case _=>false})) match {
          case None => unfold(node.outs.head.child)
          case Some(fc1) => {
            val newName = if (node == tree.rootNode) fc.name else rename(fc.name, fc.name != rootName)
            val signature = Signature(newName, getVars(fc).toList)
            signatures(node) = signature
            val result = unfold(node.outs.head.child)
            defs += FFunction(signature.name, signature.args, result)
            FCall(signature.name, signature.args)
          }
        }
      }
      
    
    case gc @ GCall(name, arg0, args) => 
      if (node.outs.isEmpty){
        val pNode = node.ancestors.find(n => n.expr match {case gc_ : GCall => equivalent(gc, gc_); case _=>false}).get
        val pGc = pNode.expr.asInstanceOf[GCall]
        val pSign = signatures(pNode)
        val sub = Map() ++ renaming(pGc, gc)
        if (pNode.outs.size == 1){
          FCall(pSign.name, pSign.args.map(applySubstitution(_, sub)))
        } else {
          GCall(pSign.name, applySubstitution(pSign.args.head, sub) , pSign.args.tail.map(applySubstitution(_, sub)))
        }
      } else if (node.outs.head.substitution.isEmpty) {
        tree.leafs.find(n => n.ancestors.contains(node) && 
        (n.expr match {case gc_ : GCall => equivalent(gc, gc_); case _=>false})) match {
          case None => unfold(node.outs.head.child)
          case Some(fc1) => {
            val signature = Signature(rename(gc.name, true), getVars(gc).toList)
            signatures(node) = signature
            val result = unfold(node.outs.head.child)
            defs += FFunction(signature.name, signature.args, result)
            result
          }
        }
      } else {
        val patternVar = node.outs.head.substitution.toList.head._1
        val vars = (getVars(gc) - patternVar).toList
        val signature = Signature(rename(gc.name, true), patternVar :: vars)
        signatures(node) = signature
        for (edge <- node.outs){
          val e = edge.substitution(patternVar).asInstanceOf[Constructor]
          val patName = e.name
          var patVars = e.args.map(_.asInstanceOf[Variable])
          defs += GFunction(signature.name, Pattern(patName, patVars), vars, unfold(edge.child))
        }
        GCall(signature.name, patternVar, vars)
      }
  }
  
  private def getVars(t: Term): Set[Variable] = t match {
    case v: Variable => Set(v)
    case c: Constructor => {
      var vars = scala.collection.mutable.Set[Variable]()
      c.args.map(arg => {vars ++ getVars(arg)})
      Set() ++ vars
    }
    case f: FCall => {
      var vars = scala.collection.mutable.Set[Variable]()
      f.args.map(arg => {vars ++ getVars(arg)})
      Set() ++ vars
    }
    case g: GCall => {
      var vars = scala.collection.mutable.Set[Variable]()
      vars ++ getVars(g.arg0)
      g.args.map(arg => {vars ++ getVars(arg)})
      Set() ++ vars
    }
  }
  
  private def rename(name: String, isOriginalNameAllowed: Boolean) = {
    if (isOriginalNameAllowed && !fnames.contains(name)){
      fnames += name
      name
    } else {
      var index = 1
      var newName = name + index
      while(fnames.contains(newName)){
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
  val digits = Array( 'a' , 'b' ,
    'c' , 'd' , 'e' , 'f' , 'g' , 'h' ,
    'i' , 'j' , 'k' , 'l' , 'm' , 'n' ,
    'o' , 'p' , 'q' , 'r' , 's' , 't' ,
    'u' , 'v' , 'w' , 'x' , 'y' , 'z');
  
  def getVar(n: Int): Variable = {
    val sb = new StringBuilder
    for (s <- Integer.toString(n, 26)) {
      sb.append(digits(Integer.parseInt("" + s, 26)))
    } 
    Variable(sb.toString)
  }
  
  def renameVarsInDefinition(d: Definition) = d match {
    case f: FFunction =>
      val args = f.args
      val renaming = Map() ++ ((args) zip (args.indices.map(getVar(_)))) 
      FFunction(f.name, f.args.map(renaming(_)), applySubstitution(f.term, renaming))
    case g: GFunction =>
      val args = g.arg0.args ::: g.args
      val renaming = Map() ++ ((args) zip (args.indices.map(getVar(_))))
      GFunction(g.name, Pattern(g.arg0.name, g.arg0.args.map(renaming(_))), g.args.map(renaming(_)), applySubstitution(g.term, renaming))    
  }

}
