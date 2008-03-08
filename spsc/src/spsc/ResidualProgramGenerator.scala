package spsc;

import SmallLanguage._
import ProcessTree._
import SmallLanguageTermAlgebra._
import Util.applySubstitution


class ResidualProgramGenerator(val tree: ProcessTree) {
  import ResidualProgramGenerator._
  
  private var signatures = scala.collection.mutable.Map[Node, Signature]()
  private val defs = new scala.collection.mutable.ListBuffer[Definition]
  
  private def generateProgram(): Program = {
    val t = unfold(tree.rootNode)
    val rootCall = tree.rootNode.expr.asInstanceOf[FCall]
    signatures.get(tree.rootNode) match {
      case None => defs += FFunction(rootCall.name, rootCall.args.map(_.asInstanceOf[Variable]), t)
      case _ =>
    }
    Program(defs.toList)
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
        val pFc = node.expr.asInstanceOf[FCall]
        val pSign = signatures(pNode)
        val sub = Map() ++ renaming(fc, pFc)
        FCall(pSign.name, pSign.args.map(applySubstitution(_, sub)))
      } else {
        tree.leafs.find(n => n.ancestors.contains(node) && 
        (n.expr match {case fc_ : FCall => equivalent(fc, fc_); case _=>false})) match {
          case None => unfold(node.outs.head.child)
          case Some(fc1) => {
            val newName = if (node == tree.rootNode) fc.name else rename(fc.name)
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
        val pGc = node.expr.asInstanceOf[GCall]
        val pSign = signatures(pNode)
        val sub = Map() ++ renaming(gc, pGc)
        if (pNode.outs.size == 1){
          FCall(pSign.name, pSign.args.map(applySubstitution(_, sub)))
        } else {
          GCall(pSign.name, applySubstitution(pSign.args.head, sub) , pSign.args.tail.map(applySubstitution(_, sub)))
        }
      } else if (node.outs.size == 1) {
        tree.leafs.find(n => n.ancestors.contains(node) && 
        (n.expr match {case gc_ : GCall => equivalent(gc, gc_); case _=>false})) match {
          case None => unfold(node.outs.head.child)
          case Some(fc1) => {
            val signature = Signature(rename(gc.name), getVars(gc).toList)
            signatures(node) = signature
            val result = unfold(node.outs.head.child)
            defs += FFunction(signature.name, signature.args, result)
            result
          }
        }
      } else {
        val patternVar = node.outs.head.substitution.toList.head._1
        val vars = (getVars(gc) - patternVar).toList
        val signature = Signature(rename(gc.name), patternVar :: vars)
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
  
  private var index = 0
  private def rename(name: String) = {
    index += 1
    name + "_" + index
  }
}

object ResidualProgramGenerator{
  case class Signature(name: String, args: List[Variable])
  def generateResidualProgram(tree: ProcessTree) = new ResidualProgramGenerator(tree).generateProgram()
}
