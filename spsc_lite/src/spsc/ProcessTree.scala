package spsc;

import SmallLanguageTermAlgebra._

object ProcessTree {
  def apply(expr: Expression) = 
    new ProcessTree(new Node(expr, null, Nil))
  
  class Node(val expr: Expression, val in: Edge, var outs: List[Edge]) {   
    def ancestors(): List[Node] = if (in == null) Nil else in.parent :: in.parent.ancestors

    def isProcessed: Boolean = expr match {
      case Constructor(_, Nil) => true
      case v : Variable => true
      case l: LetExpression => false
      case _ => {
        var edge = in
        while (edge != null) {
          val node1 = edge.parent
          if (!isTrivial(node1.expr) && equivalent(expr.asInstanceOf[Term], node1.expr.asInstanceOf[Term])) return true
          edge = node1.in
        }
        false
      }
    }
    
    def getRepParent(): Node = expr match {
      case Constructor(_, _) => null
      case v : Variable => null
      case l: LetExpression => null
      case _ => {
        var edge = in
        while (edge != null) {
          val node1 = edge.parent
          if (!isTrivial(node1.expr) && equivalent(expr.asInstanceOf[Term], node1.expr.asInstanceOf[Term])) return node1
          edge = node1.in
        }
        null
      }
    }
  }
  
  class Edge(val parent: Node, var child: Node, val substitution: Map[Variable, Term])
}

import ProcessTree._
class ProcessTree {
  var rootNode: Node = null
  private var leafs_ = List[Node]()
  
  def this(root: Node) {
   this()
   rootNode = root
   leafs_ = List[Node](rootNode)
  }
  
  def leafs = leafs_
  
  def addChildren(node: Node, children: List[Pair[Term, Map[Variable, Term]]]) = {
    assume(leafs_.contains(node))
    leafs_ = leafs_.remove(_ == node)
    val edges = new scala.collection.mutable.ListBuffer[Edge]
    for (pair <- children){
      val edge = new Edge(node, null, pair._2)
      val childNode = new Node(pair._1, edge, Nil)
      leafs_ = childNode :: leafs
      edge.child = childNode
      edges += edge
    }
    node.outs = edges.toList
  }
  
  def replace(node: Node, exp: Expression) = {
    // the node can be not leaf - but from any part of tree
    leafs_ = leafs_.remove(_ == node)
    leafs_ = leafs_.remove(_.ancestors.contains(node))
    val childNode = new Node(exp, node.in, Nil)
    // the node can be root node:
    if (node == rootNode){
      rootNode = childNode
    } else {
      node.in.child = childNode
    }
    leafs_ = childNode :: leafs
  }
  
  def isClosed = leafs_.forall(_.isProcessed)
  
  override def toString = rootNode.toString
}


