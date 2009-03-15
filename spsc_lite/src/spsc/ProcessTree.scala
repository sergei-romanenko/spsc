package spsc

class Edge(val parent: Node, var child: Node, val substitution: (Var, Cons))
class Node(val expr: Term, val in: Edge, var outs: List[Edge]) {
  var repeated: Node = null
  def ancestors(): List[Node] = if (in == null) Nil else in.parent :: in.parent.ancestors
  def leafs(): List[Node] = 
    if (outs.isEmpty) List(this) else List.flatten((outs map {_.child.leafs()}))
  def isProcessed: Boolean = expr match {
    case Cons(_, Nil) => true
    case v: Var => true
    case _ => repeated != null
  }
}
class Tree(var root: Node) {
  def leafs = root.leafs
  def replace(node: Node, exp: Term) =  node.in.child = new Node(exp, node.in, Nil)
  
  def addChildren(node: Node, children: List[(Term, (Var, Cons))]) =
    node.outs = for ((term, b) <- children) yield {
      val edge = new Edge(node, null, b)
      edge.child = new Node(term, edge, Nil)
      edge
    }
}