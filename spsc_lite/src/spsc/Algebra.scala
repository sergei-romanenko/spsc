package spsc

object Algebra {
  def sub(term: Term, map: Map[Var, Term]): Term = term match {
    case v: Var => map.getOrElse(v, v)
    case Cons(n, vs)  => Cons(n,  vs.map(sub(_, map)))
    case FCall(n, vs) => FCall(n, vs.map(sub(_, map)))
    case GCall(n, vs) => GCall(n, vs.map(sub(_, map)))
  }
  def equiv(t1: Term, t2: Term): Boolean = inst(t1, t2) && inst(t2, t1)
  def inst(t1: Term, t2: Term): Boolean = findSub(t1, t2) != null
  def findSub(t1: Term, t2: Term): Map[Var, Term] = {
    val map = scala.collection.mutable.Map[Var, Term]()
    def walk(t1: Term, t2: Term): Boolean = (t1, t2) match {
      case (v1: Var, _) => map.getOrElse(v1, t2) == (map+(v1 -> t2))(v1) 
      case (Cons(n1, xs),  Cons(n2, ys))  => n1 == n2 && List.forall2(xs, ys)(walk)
      case (FCall(n1, xs), FCall(n2, ys)) => n1 == n2 && List.forall2(xs, ys)(walk)
      case (GCall(n1, xs), GCall(n2, ys)) => n1 == n2 && List.forall2(xs, ys)(walk)
      case _ => false
    }
    if (walk(t1, t2)) Map(map.toList:_*).filter{case (k, v) => k != v} else null
  }
  def vars(t: Term): List[Var] = t match {
    case v: Var   => (List(v))
    case c: Cons  => (List[Var]() /: c.args) {case (l, a) => l union vars(a)}
    case f: FCall => (List[Var]() /: f.args) {case (l, a) => l union vars(a)}
    case g: GCall => (List[Var]() /: g.args) {case (l, a) => l union vars(a)}
  }
  def he(t1: Term, t2: Term): Boolean = heByVar(t1, t2) || heByDiving(t1, t2) || heByCoupling(t1, t2)  
  private def heByVar(t1: Term, t2: Term): Boolean = (t1, t2) match {
    case (Var(_), Var(_)) => true
    case _ => false
  }
  private def heByDiving(t1: Term, t2: Term): Boolean = t2 match {
    case (Cons(_, args)) => args exists (he(t1, _))
    case (FCall(_, args)) => args exists (he(t1, _))
    case (GCall(_, args)) => args exists (he(t1, _))
    case _ => false
  }
  private def heByCoupling(t1: Term, t2: Term): Boolean = (t1, t2) match {
    case (Cons(n1, args1), Cons(n2, args2)) if n1 == n2 => List.forall2(args1, args2)(he)
    case (FCall(n1, args1), FCall(n2, args2)) if n1 == n2 => List.forall2(args1, args2)(he)
    case (GCall(n1, args1), GCall(n2, args2)) if n1 == n2 => List.forall2(args1, args2)(he)
    case _ => false
  }
}