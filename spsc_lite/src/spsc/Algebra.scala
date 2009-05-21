package spsc

case class Gen(t: Term, m1: Map[Var, Term], m2: Map[Var, Term])

object Algebra {
  
  def shallowEq(e1: CFGTerm, e2: CFGTerm) =
    e1.kind == e2.kind && e1.name == e2.name
  
  def subst(term: Term, m: Map[Var, Term]): Term = term match {
    case v: Var     => m.getOrElse(v, v)
    case e: CFGTerm => e.replaceArgs(e.args.map(subst(_, m)))
  }
  
  def equiv(t1: Term, t2: Term): Boolean = inst(t1, t2) && inst(t2, t1)
  
  def inst(t1: Term, t2: Term): Boolean = findSubst(t1, t2) != null
  
  def findSubst(t1: Term, t2: Term): Map[Var, Term] = {
    val map = scala.collection.mutable.Map[Var, Term]()
    def walk(t1: Term, t2: Term): Boolean = (t1, t2) match {
      case (v1: Var, _) => map.getOrElse(v1, t2) == (map+(v1 -> t2))(v1)
      case (e1: CFGTerm, e2:CFGTerm) if shallowEq(e1, e2) =>
        List.forall2(e1.args, e2.args)(walk)
      case _ => false
    }
    if (walk(t1, t2)) Map(map.toList:_*).filter{case (k, v) => k != v} else null
  }
  
  def vars(t: Term): List[Var] = t match {
    case v: Var   => (List(v))
    case e: CFGTerm => (List[Var]() /: e.args) {_ union vars(_)}
  }
  
  def he_*(t1: Term, t2: Term): Boolean = he(t1, t2) && b(t1) == b(t2)
  
  def he(t1: Term, t2: Term): Boolean = heByDiving(t1, t2) || heByCoupling(t1, t2)
  
  private def heByDiving(t1: Term, t2: Term): Boolean = t2 match {
    case e: CFGTerm => e.args exists (he(t1, _))
    case _ => false
  }
  
  private def heByCoupling(t1: Term, t2: Term): Boolean = (t1, t2) match {
    case (e1:CFGTerm, e2:CFGTerm) if shallowEq(e1, e2) => List.forall2(e1.args, e2.args)(he)
    case (Var(_), Var(_)) => true
    case _ => false
  }
  
  def msg(t1: Term, t2: Term): Gen = {
    val v = freshVar()
    var g = Gen(v, Map(v -> t1), Map(v -> t2))
    var exp = g.t
    do {exp = g.t; g = commonSubst(commonFun(g))} while (exp != g.t)
    g
  }
  
  def commonFun(g: Gen): Gen = {
    for (v <- g.m1.keys) (g.m1(v), g.m2(v)) match {
      case (e1:CFGTerm, e2:CFGTerm) if shallowEq(e1, e2) =>
        val vs = e1.args map freshVar
        val t = subst(g.t, Map(v -> e1.replaceArgs(vs)))
        return Gen(t, (g.m1 - v) ++ vs.zip(e1.args), (g.m2 - v) ++ vs.zip(e2.args))        
      case _ =>
    }
    g
  }
  
  def commonSubst(gen: Gen): Gen = {
    for ((v1, e1) <- gen.m1; (v2, e2) <- gen.m1)
      if ((v1 != v2 && e1 == e2) && (gen.m2(v1) == gen.m2(v2)))
        return Gen(subst(gen.t, Map(v1 -> v2)), gen.m1 - v1, gen.m2 - v1)
    gen
  }
  
  def freshVar(x: AnyRef) = {i += 1; Var("v" + i)}; private var i = 0;
                                              
  private def b(t: Term): Int = t match {
    case GCall(_, args) => b(args(0))
    case Var(_) => 1
    case _ => 0
  }
  
  def trivial(expr: Term): Boolean = expr match {
    case x: Call => false
    case _ => true
  }
}