package spsc
import Algebra._
class SuperCompiler(p: Program){
  def driveExp(expr: Term): List[(Term, Branch)] = expr match {
    case Ctr(name, args) => args.map((_,null))
    case FCall(name, args)  => List((sub(p.f(name).term, Map(p.f(name).args.zip(args): _*)), null))
    case GCall(name, Ctr(cname, cargs) :: args) =>
      val g = p.g(name, cname)  
      List((sub(g.term, Map((g.p.args:::g.args) zip (cargs ::: args): _*)), null))
    case gCall @ GCall(name, (v : Var) :: args) => 
      for (g <- p.gs(name); val pat = freshPat(g.p); val cons = Ctr(pat.name, pat.args))
        yield (driveExp(sub(gCall, Map(v -> cons)))(0)._1, Branch(v, pat))
    case GCall(name, call :: args) => driveExp(call) map {p => (GCall(name, p._1 :: args), p._2)}
    case Let(term, bs) => (term, null) :: bs.map {pair => (pair._2, null)}
  }
  def buildProcessTree(e: Term): Tree = {
    val t = new Tree(new Node(e, null, Nil))
    while (!t.leafs.forall{_.isProcessed}) {
      println(t)
      println()
      val b = t.leafs.find(!_.isProcessed).get
      if (trivial(b.expr)) {
        t.addChildren(b, driveExp(b.expr)) //drive
      } else {
        b.ancestors.find(a => !trivial(a.expr) && he_*(a.expr, b.expr)) match {
          case Some(a) => { 
            if (equiv(a.expr, b.expr)) b.fnode = a 
            else if (inst(a.expr, b.expr)) abs(t, b, a)
            else if (equiv(msg(a.expr, b.expr).t, Var("z"))) split(t, b)
            else abs(t, a, b)
          }
          case None => t.addChildren(b, driveExp(b.expr)) // drive
        }
      }
    }   
    t
  }
  def abs(t: Tree, a: Node, b: Node) =
    ((g: Gen) => t.replace(a, Let(g.t, g.m1.toList))) (msg(a.expr, b.expr))
  def trivial(expr: Term): Boolean = expr match {case x: Call => false; case _ => true}
  private def freshPat(p: Pattern) = Pattern(p.name, p.args map nv)
  def split(t: Tree, n: Node) = n.expr match {
    case Ctr(cn, xs)  => ((vs: List[Var]) => t.replace(n, Let(Ctr(cn, vs), vs zip xs)))  (xs map nv)
    case FCall(fn, xs) => ((vs: List[Var]) => t.replace(n, Let(FCall(fn, vs), vs zip xs))) (xs map nv)
    case GCall(gn, xs) => ((vs: List[Var]) => t.replace(n, Let(GCall(gn, vs), vs zip xs))) (xs map nv)
  }
}