package spsc

import Algebra._
case class Gen(t: Term, m1: Map[Var, Term], m2: Map[Var, Term])
object MSG {
  def msg(t1: Term, t2: Term): Gen = {
    val v = freshVar()
    var g = Gen(v, Map(v -> t1), Map(v -> t2))
    var exp = g.t
    do {exp = g.t; g = commonSubst(commonFun(g))} while (exp != g.t)
    g
  }
  
  def commonFun(g: Gen): Gen = {
    for (v <- g.m1.keys) (g.m1(v), g.m2(v)) match {
      case (e1:CFG, e2:CFG) if shellEq(e1, e2) =>
        val vs = e1.args map freshVar
        val t = applySubst(Map(v -> e1.replaceArgs(vs)), g.t)
        return Gen(t, g.m1 - v ++ vs.zip(e1.args), g.m2 - v ++ vs.zip(e2.args))        
      case _ =>
    }
    g
  }
  
  def commonSubst(gen: Gen): Gen = {
    for ((v1, e1) <- gen.m1; (v2, e2) <- gen.m1)
      if ((v1 != v2 && e1 == e2) && (gen.m2(v1) == gen.m2(v2)))
        return Gen(applySubst(Map(v1 -> v2), gen.t), gen.m1 - v1, gen.m2 - v1)
    gen
  }
}
