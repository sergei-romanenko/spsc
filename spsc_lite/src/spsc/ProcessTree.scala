package spsc

case class Branch(v: Var, pat: Pattern)
class Edge(val parent: Node, var child: Node, val branch: Branch)
class Node(val expr: Term, val in: Edge, var outs: List[Edge]) {
  var repeated: Node = null
  def ancestors(): List[Node] = if (in == null) Nil else in.parent :: in.parent.ancestors
  def leafs(): List[Node] = if (outs.isEmpty) List(this) else List.flatten(children map {_.leafs})
  def children : List[Node] = outs map {_.child}
  def isProcessed: Boolean = expr match {
    case Cons(_, Nil) => true
    case v: Var => true
    case _ => repeated != null
  }
}
class Tree(var root: Node) {
  def leafs = root.leafs
  def replace(node: Node, exp: Term) =  node.in.child = new Node(exp, node.in, Nil)
  def addChildren(node: Node, children: List[(Term, Branch)]) =
    node.outs = for ((term, b) <- children) yield {
      val edge = new Edge(node, null, b)
      edge.child = new Node(term, edge, Nil)
      edge
    }
}