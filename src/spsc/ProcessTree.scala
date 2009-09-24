package spsc
import Algebra._

case class Contraction(v: Var, pat: Pat) {
  override def toString = v.toString + "=" + pat.toString
}

class Node(val nodeId : Int, val expr: Term, val parent: Node, val contr: Contraction) {
  
  def ancestors: List[Node] = 
    if (parent == null) Nil else parent :: parent.ancestors
  
  def isProcessed = expr match {
    case Ctr(_, Nil) => true
    case v: Var => true
    case _ => funcAncestor != null
  }
  
  def funcAncestor =
    ancestors.find{n => isFGCall(n.expr) && equiv(expr, n.expr)}.getOrElse(null)
}

class Tree(val freeId : Int, val root: Node, val children: Map[Node, List[Node]]) {
  
  def addChildren(n: Node, cs: List[(Term, Contraction)]) = {
    var i = freeId - 1
    new Tree(freeId+cs.length, root,
             children + (n -> (cs map {case (t, b) => i += 1; new Node(i, t, n, b)})))
    }

  def replace(n: Node, exp: Term) = 
    if (n == root) new Tree(freeId, n, Map().withDefaultValue(Nil))
    else {
      val p = n.parent
      val cs = children(p) map {m => if (m == n) new Node(freeId, exp, p, n.contr) else m}
      new Tree(freeId+1, root, children + (p -> cs))
    }
  
  def leaves_(node: Node): List[Node] = 
    if (children(node).isEmpty) List(node) 
    else List.flatten(children(node) map leaves_)
  
  def leaves() = leaves_(root)

  override def toString() : String = {
    val acc = new StringBuilder()
    def walk(n : Node) : Unit = {
      val parentId_s = if( n.parent == null ) "" else n.parent.nodeId.toString
      val children_s = (for (child <- children(n)) yield child.nodeId.toString).mkString(",")
      val contr_s = if( n.contr == null ) "" else n.contr.toString
      val node_s = n.nodeId.toString + ":(" + n.expr + "," + contr_s +
        "," + parentId_s + ",[" + children_s + "])"
      if( acc.length > 0 ) acc.append(",")
      acc.append(node_s)
      for( child <- children(n)) walk(child)
    }
    acc.append("{")
    walk(root)
    acc.append("}")
    acc.toString
  }
}

object Tree {

  def create(e : Term) =
    new Tree(1, new Node(0, e, null, null), Map().withDefaultValue(Nil))
}