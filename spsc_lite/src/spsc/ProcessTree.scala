package spsc;

import SmallLanguageTermAlgebra._

object ProcessTree {
  def apply(expr: Expression) = 
    new ProcessTree(new Node(expr, null, Nil))
  
  class Node(val expr: Expression, val in: Edge, var outs: List[Edge]) {
    var repeated: Node = null
    def ancestors(): List[Node] = if (in == null) Nil else in.parent :: in.parent.ancestors
    
    def isProcessed: Boolean = expr match {
      case Constructor(_, Nil) => true
      case v : Variable => true
      case _ => repeated != null
    }
    
    def leafs(): List[Node]= outs match {
      case Nil => this :: Nil
      case _ => List.flatten(outs map {_.child.leafs()})
    }
  }
  
  class Edge(val parent: Node, var child: Node, val substitution: Map[Variable, Term])
}

import ProcessTree._
class ProcessTree(var root: Node) {
  var leafs = root :: Nil
  
  def addChildren(node: Node, children: List[Pair[Term, Map[Variable, Term]]]) = {
    leafs = leafs.remove(_ == node)
    node.outs = for (pair <- children) yield {
      val edge = new Edge(node, null, pair._2)
      val childNode = new Node(pair._1, edge, Nil)
      leafs = childNode :: leafs
      edge.child = childNode
      edge
    }
  }
  
  def replace(node: Node, exp: Expression) = {
    // the node can be not leaf - but from any part of tree
    leafs = leafs.remove(_ == node)
    leafs = leafs.remove(_.ancestors.contains(node))
    val childNode = new Node(exp, node.in, Nil)
    // the node can be root node:
    if (node == root){
      root = childNode
    } else {
      node.in.child = childNode
    }
    leafs = childNode :: leafs
  }
  
  def isClosed = leafs.forall(_.isProcessed)
}