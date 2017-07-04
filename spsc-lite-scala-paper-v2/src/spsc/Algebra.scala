package spsc

object Algebra {

  def subst(term: Term, m: Map[Var, Term]): Term = term match {
    case v: Var => m.getOrElse(v, v)
    case Ctr(name, args) => Ctr(name, args map { subst(_, m) })
    case FCall(name, args) => FCall(name, args map { subst(_, m) })
    case GCall(name, args) => GCall(name, args map { subst(_, m) })
  }

  def equiv(t1: Term, t2: Term): Boolean = inst(t1, t2) && inst(t2, t1)

  def inst(t1: Term, t2: Term): Boolean = findSubst(t1, t2) != null

  def findSubst(t1: Term, t2: Term): Map[Var, Term] = {
    val map = scala.collection.mutable.Map[Var, Term]()
    def walk(t1: Term, t2: Term): Boolean = (t1, t2) match {
      case (v1: Var, _) => map.getOrElse(v1, t2) == (map + (v1 -> t2))(v1)
      case (Ctr(n1, args1), Ctr(n2, args2)) => n1 == n2 && List.forall2(args1, args2)(walk)
      case (FCall(n1, args1), FCall(n2, args2)) => n1 == n2 && List.forall2(args1, args2)(walk)
      case (GCall(n1, args1), GCall(n2, args2)) => n1 == n2 && List.forall2(args1, args2)(walk)
      case _ => false
    }
    if (walk(t1, t2)) Map(map.toList: _*).filter { case (k, v) => k != v } else null
  }

  def vars(t: Term): List[Var] = t match {
    case v: Var => (List(v))
    case Ctr(_, args) => (List[Var]() /: args) { (vs, exp) => vs ++ (vars(exp) -- vs) }
    case FCall(_, args) => (List[Var]() /: args) { (vs, exp) => vs ++ (vars(exp) -- vs) }
    case GCall(_, args) => (List[Var]() /: args) { (vs, exp) => vs ++ (vars(exp) -- vs) }
  }

  def freshVar(x: AnyRef = null) = { i += 1; Var("v" + i) }; private var i = 0;

  def trivial(expr: Term): Boolean = expr match {
    case FCall(_, _) => false
    case GCall(_, _) => false
    case _ => true
  }
}