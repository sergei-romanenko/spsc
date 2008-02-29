package spsc;

object TermAlgebra {
  sealed abstract class ATerm
  case class AVar(name: String) extends ATerm {
    override def toString = name
  }
  case class ASym(name: String, args: List[ATerm]) extends ATerm {
    override def toString = name + args.mkString("(",",", ")") 
  }
  
  def he(term1: ATerm, term2: ATerm): Boolean = 
    heByVar(term1, term2) || heByDiving(term1, term2) || heByCoupling(term1, term2)
  
  private def heByVar(term1: ATerm, term2: ATerm): Boolean = (term1, term2) match {
    case (AVar(_), AVar(_)) => true
    case _ => false
  }
  
  private def heByDiving(term1: ATerm, term2: ATerm): Boolean = term2 match {
    case (ASym(_, args)) => args exists (he(term1, _))
    case _ => false
  }
  
  private def heByCoupling(term1: ATerm, term2: ATerm): Boolean = (term1, term2) match {
    case (ASym(name1, args1), ASym(name2, args2)) if name1 == name2 && args1.size == args2.size => 
      (args1 zip args2) forall (args => he(args._1, args._2)) 
    case _ => false
  }
  
  type Substitution = Tuple2[AVar, ATerm]
  type DoubleSubstitution = Tuple3[AVar, ATerm, ATerm]
  
  //case class Generalization(term: ATerm, sub1: List[Substitution], sub2: List[Substitution])
  case class Generalization2(term: ATerm, dSub: List[DoubleSubstitution])
  
  def msg(term1: ATerm, term2: ATerm): Generalization2 = {
    val initialVar = nextVar()
    var g = Generalization2(initialVar, List((initialVar, term1, term2)))
    var exp = g.term
    do {
      exp = g.term
      g = applyCommonFunctorRule(g)
      g = applyCommonSubExpressionRule(g)
    } while (exp != g.term)
    g
  }
  
  private def applyCommonFunctorRule(g: Generalization2): Generalization2 = {
    val l2 = new scala.collection.mutable.ListBuffer[DoubleSubstitution]()
    var t = g.term;
    for (dSub <- g.dSub) dSub match {
      case (v, s1 @ ASym(name1, args1), s2 @ ASym(name2, args2)) if name1 == name2 => {
        val newVars = args1.map(arg => nextVar())
        val addDSubs = ((newVars zip args1) zip (newVars zip args2)) map (pair => (pair._1._1, pair._1._2, pair._2._2)) 
        t = applySubstitution(t, (v, ASym(name1, newVars)))
        l2 ++= addDSubs
      }         
      case d => l2 += d 
    }
    Generalization2(t, l2.toList)
  }
  
  private def f1(ds: DoubleSubstitution, p: Pair[List[DoubleSubstitution], List[DoubleSubstitution]]) = p match {
    case (Nil, l) => l.partition(triple => triple._2 == ds._2 && triple._3 == ds._3) match {
      case (Nil, _) => (Nil, ds :: l) 
      case (same, dif) => (ds :: same, dif)
    }
    case (l1 @ s :: _, l2) => if (ds._2 == s._2 && ds._3 == s._3) (ds :: l1, l2) else (l1, ds :: l2)
  } 
   
  private def applyCommonSubExpressionRule(g: Generalization2): Generalization2 = {
    g.dSub.foldRight((List[DoubleSubstitution](), List[DoubleSubstitution]()))(f1) match {
      case (Nil, _) => g
      case ((s @ (v, _, _)) :: o1, o2) => 
        Generalization2(o1.foldRight(g.term)((ds, t) => applySubstitution(t, (ds._1, v))), s :: o2)
    }
  }
  
  private def applySubstitution(t: ATerm, sub: Substitution): ATerm = t match {
    case v: AVar => if (v == sub._1) sub._2 else v
    case ASym(name, args) => ASym(name, args.map(applySubstitution(_, sub)))
  }
  
  def equivalent(term1: ATerm, term2: ATerm): Boolean = {
    var map1to2 = scala.collection.mutable.Map[AVar, AVar]()
    var map2to1 = scala.collection.mutable.Map[AVar, AVar]()
    def eq1(t1: ATerm, t2: ATerm): Boolean = (t1, t2) match {
      case (v1: AVar, v2: AVar) => (map1to2.get(v1), map2to1.get(v2)) match {
        case (Some(v3), Some(v4)) => v2 == v3 && v1 == v4
        case (None, None) => map1to2(v1) = v2; map2to1(v2) = v1; true
        case _ => false
      }
      case (ASym(name1, args1), ASym(name2, args2)) if name1 == name2 =>
        ((args1 zip args2) forall (args => eq1(args._1, args._2)))
      case _ => false
    }
    eq1(term1, term2)
  }
  
  // test whether t2 is instance of t1
  // t1 <~ t2 => t1{theta} = t2
  def instanceOf(t1: ATerm, t2: ATerm): Boolean = equivalent(msg(t1, t2).term, t1)
  
  private var counter = 0
  private def nextVar(): AVar = {
    counter += 1
    AVar("x_" + counter)
  }
}
