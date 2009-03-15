package spsc

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
  
class Edge(val parent: Node, var child: Node, val substitution: Map[Var, Term])

class ProcessTree(var root: Node) {
  def leafs = root.leafs
  def replace(node: Node, exp: Term) =  node.in.child = new Node(exp, node.in, Nil)
  def isClosed = leafs.forall(_.isProcessed)
  
  def addChildren(node: Node, children: List[Pair[Term, Map[Var, Term]]]) =
    node.outs = for (pair <- children) yield {
      val edge = new Edge(node, null, pair._2)
      val childNode = new Node(pair._1, edge, Nil)
      edge.child = childNode
      edge
    }
}