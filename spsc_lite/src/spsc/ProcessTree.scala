package spsc
import Algebra._

case class Branch(v: Var, pat: Pattern)
class Node(val expr: Term, val parent: Node, val branch: Branch) {
  def ancestors(): List[Node] = if (parent == null) Nil else parent :: parent.ancestors
  
  def isProcessed: Boolean = expr match {
    case Ctr(_, Nil) => true
    case v: Var => true
    case _ => fnode != null
  }
  
  def fnode() = (ancestors find {n => !trivial(n.expr) && equiv(expr, n.expr)}).getOrElse(null)
}

class Tree(val root: Node, val children: Map[Node, List[Node]]) {
  
  def addChildren(node: Node, cs: List[(Term, Branch)]) = 
    new Tree(root, children + (node -> (cs map {case (t, b) => new Node(t, node, b)})))

  def replace(n: Node, exp: Term) = 
    if (n == root) new Tree(n, Map().withDefaultValue(Nil))
    else new Tree(root, children + (n -> ( children(n.parent) map {m => if (m == n) new Node(exp, n.parent, n.branch) else m} ) )) 
  
  def leafs_(node: Node): List[Node] = 
    if (children(node).isEmpty) List(node) else List.flatten(children(node) map leafs_)
  
  def leafs() = leafs_(root)
  
  def toString(node: Node, indent: String): String = {
    val sb = new StringBuilder(indent + "|__" + node.expr)
    for (n <- children(node)) {
      sb.append("\n  " + indent + "|" + n.branch)
      sb.append("\n" + toString(n, indent + "  "))
    }
    sb.toString
  }
  
  override def toString = toString(root, "")
}