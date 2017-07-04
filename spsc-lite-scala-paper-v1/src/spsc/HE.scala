package spsc

import Algebra._

object HE {
  def he_*(t1: Term, t2: Term): Boolean = he(t1, t2) && b(t1) == b(t2)
  
  def he(t1: Term, t2: Term) = heByDiving(t1, t2) || heByCoupling(t1, t2)
  
  private def heByDiving(t1: Term, t2: Term): Boolean = t2 match {
    case e: CFG => e.args exists (he(t1, _))
    case _ => false
  }
  
  private def heByCoupling(t1: Term, t2: Term): Boolean = (t1, t2) match {
    case (e1:CFG, e2:CFG) if shallowEq(e1, e2) => List.forall2(e1.args, e2.args)(he)
    case (Var(_), Var(_)) => true
    case _ => false
  }
  
  private def b(t: Term): Int = t match {
    case GCall(_, args) => b(args.head)
    case Var(_) => 1
    case _ => 0
  } 
}