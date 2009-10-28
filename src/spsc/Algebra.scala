package spsc

object Algebra {
  
  def shellEq(e1: CFG, e2: CFG) =
    e1.kind == e2.kind &&
    e1.name == e2.name &&
    e1.args.length == e2.args.length
  
  def applySubst(m: Map[Var, Term], term: Term): Term = term match {
    case v: Var => m.getOrElse(v, v)
    case e: CFG => e.replaceArgs(e.args.map(applySubst(m, _)))
  }
  
  def equiv(t1: Term, t2: Term): Boolean = instOf(t1, t2) && instOf(t2, t1)
  
  def instOf(t1: Term, t2: Term): Boolean = matchAgainst(t2, t1) != null
  
  def matchAgainst(t1: Term, t2: Term) = {
    var map = Map[Var, Term]()
    def walk(t1: Term, t2: Term): Boolean = (t1, t2) match {
      case (v1: Var, _) => map.get(v1) match {
        case None => map += (v1 -> t2); true 
        case Some(t3) => t2 == t3
      }
      case (e1: CFG, e2:CFG) if shellEq(e1, e2) => 
        List.forall2(e1.args, e2.args)(walk)
      case _ => false
    }
    if (walk(t1, t2)) map else null
  }
  
  def vars(t: Term): List[Var] = t match {
    case v: Var => (List(v))
    case e: CFG => (List[Var]() /: e.args) {(vs, exp) =>  vs ++ (vars(exp) -- vs)}
  }
  
  private var i = 0;

  def resetVarGen() { i = 0; }
  
  def freshVar(x: AnyRef) = {i += 1; Var("v" + i)};
  
  def isFGCall(expr: Term): Boolean = expr match {
    case FCall(_, _) => true
    case GCall(_, _) => true
    case _ => false
  }
}