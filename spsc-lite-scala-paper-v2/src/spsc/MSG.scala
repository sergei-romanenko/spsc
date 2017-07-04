package spsc

import Algebra._
case class Gen(t: Term, m1: Map[Var, Term], m2: Map[Var, Term])
object MSG {
  def msg(t1: Term, t2: Term): Gen = {
    val v = freshVar()
    var g = Gen(v, Map(v -> t1), Map(v -> t2))
    var exp = g.t
    do { exp = g.t; g = commonSubst(commonFun(g)) } while (exp != g.t)
    g
  }

  def commonFun(g: Gen): Gen = {
    for (v <- g.m1.keys) (g.m1(v), g.m2(v)) match {
      case (Ctr(n1, args1), Ctr(n2, args2)) if n1 == n2 => {
        val vs = args1 map freshVar
        val t = subst(g.t, Map(v -> Ctr(n1, vs)))
        return Gen(t, g.m1 - v ++ vs.zip(args1), g.m2 - v ++ vs.zip(args2))
      }
      case (FCall(n1, args1), FCall(n2, args2)) if n1 == n2 => {
        val vs = args1 map freshVar
        val t = subst(g.t, Map(v -> FCall(n1, vs)))
        return Gen(t, g.m1 - v ++ vs.zip(args1), g.m2 - v ++ vs.zip(args2))
      }
      case (GCall(n1, args1), GCall(n2, args2)) if n1 == n2 => {
        val vs = args1 map freshVar
        val t = subst(g.t, Map(v -> GCall(n1, vs)))
        return Gen(t, g.m1 - v ++ vs.zip(args1), g.m2 - v ++ vs.zip(args2))
      }
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
}
