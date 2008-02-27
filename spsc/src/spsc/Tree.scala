package spsc;

import SmallLanguage._

object Tree {
  case class Node(expr: Expression, in: Edge, var outs: List[Edge]) {
    override def toString = "Node("+ expr + ", " + outs.mkString("(", ", ", ")") + ")"
  }

  case class Edge(parent: Node, var child: Node, substitution: Map[Variable, Term]) {
    override def toString = "Edge("+ substitution + ", " + child + ")"
  }
}
