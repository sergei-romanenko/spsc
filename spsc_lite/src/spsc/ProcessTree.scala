package spsc

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
    case _ => List.flatten((outs map {_.child.leafs()}):List[List[Node]])
  }   
}
  
class Edge(val parent: Node, var child: Node, val substitution: Map[Variable, Term])

class ProcessTree(var root: Node) {
  def leafs = root.leafs
  def addChildren(node: Node, children: List[Pair[Term, Map[Variable, Term]]]) =
    node.outs = for (pair <- children) yield {
      val edge = new Edge(node, null, pair._2)
      val childNode = new Node(pair._1, edge, Nil)
      edge.child = childNode
      edge
    }
  
  def replace(node: Node, exp: Expression) =  
    if (node == root)
      root = new Node(exp, node.in, Nil)
    else
      node.in.child = new Node(exp, node.in, Nil)
  
  def isClosed = leafs.forall(_.isProcessed)
}