package spsc

object SmallLanguageTermAlgebra {
  type Substitution = Tuple2[Variable, Term]
  type DoubleSubstitution = Tuple3[Variable, Term, Term]
  
  case class Generalization(term: Term, sub1: List[Substitution], sub2: List[Substitution])
  case class Generalization2(term: Term, dSub: List[DoubleSubstitution])
  
  def strictHE(t1: Term, t2: Term): Boolean = he(t1, t2) && b(t1) == b(t2)
  def he(term1: Term, term2: Term): Boolean =
    heByVar(term1, term2) || heByDiving(term1, term2) || heByCoupling(term1, term2)
    
  private def heByVar(term1: Term, term2: Term): Boolean = (term1, term2) match {
    case (Variable(_), Variable(_)) => true
    case _ => false
  }
  
  private def heByDiving(term1: Term, term2: Term): Boolean = term2 match {
    case (Constructor(_, args)) => args exists (he(term1, _))
    case (FCall(_, args)) => args exists (he(term1, _))
    case (GCall(_, args)) => (args exists (he(term1, _)))
    case _ => false
  }
  
  private def heByCoupling(term1: Term, term2: Term): Boolean = (term1, term2) match {
    case (Constructor(name1, args1), Constructor(name2, args2)) if name1 == name2 => 
      (args1 zip args2) forall (args => he(args._1, args._2))      
    case (FCall(name1, args1), FCall(name2, args2)) if name1 == name2 => 
      (args1 zip args2) forall (args => he(args._1, args._2))
    case (GCall(name1, args1), GCall(name2, args2)) if name1 == name2 => 
      (args1 zip args2) forall (args => he(args._1, args._2))
    case _ => false
  }
  
  def msg(term1: Term, term2: Term): Generalization2 = {
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
  
  def strongMsg(term1: Term, term2: Term): Generalization = {
    val g = msg(term1, term2)
    val s1 = g.dSub.map(triple => (triple._1, triple._2))
    val s2 = g.dSub.map(triple => (triple._1, triple._3))
    Generalization(g.term, s1, s2)
  }
  
  private def applyCommonFunctorRule(g: Generalization2): Generalization2 = {
    val l2 = new scala.collection.mutable.ListBuffer[DoubleSubstitution]()
    var t = g.term;
    for (dSub <- g.dSub) dSub match {
      case (v, s1 @ Constructor(name1, args1), s2 @ Constructor(name2, args2)) if name1 == name2 => {
        val newVars = args1.map(arg => nextVar())
        val addDSubs = ((newVars zip args1) zip (newVars zip args2)) map (pair => (pair._1._1, pair._1._2, pair._2._2)) 
        t = applySub(t, (v, Constructor(name1, newVars)))
        l2 ++= addDSubs
      }
      case (v, s1 @ FCall(name1, args1), s2 @ FCall(name2, args2)) if name1 == name2 => {
        val newVars = args1.map(arg => nextVar())
        val addDSubs = ((newVars zip args1) zip (newVars zip args2)) map (pair => (pair._1._1, pair._1._2, pair._2._2)) 
        t = applySub(t, (v, FCall(name1, newVars)))
        l2 ++= addDSubs
      }
      case (v, s1 @ GCall(name1, arg01 :: args1), s2 @ GCall(name2, arg02 :: args2)) if name1 == name2 => {
        val newVars = (arg01 :: args1).map(arg => nextVar())
        val addDSubs = ((newVars zip (arg01 :: args1)) zip (newVars zip (arg02 :: args2))) map (pair => (pair._1._1, pair._1._2, pair._2._2)) 
        t = applySub(t, (v, GCall(name1, newVars.head :: newVars.tail)))
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
        Generalization2(o1.foldRight(g.term)((ds, t) => applySub(t, (ds._1, v))), s :: o2)
    }
  }
  
  private def applySub(t: Term, sub: Substitution): Term = t match {
    case v: Variable => if (v == sub._1) sub._2 else v
    case Constructor(name, args) => Constructor(name, args.map(applySub(_, sub)))
    case FCall(name, args) => FCall(name, args.map(applySub(_, sub)))
    case GCall(name, arg0 :: args) => GCall(name, applySub(arg0, sub) :: args.map(applySub(_, sub)))
  }
  
  def equivalent(term1: Term, term2: Term): Boolean = {
    var map1to2 = scala.collection.mutable.Map[Variable, Variable]()
    var map2to1 = scala.collection.mutable.Map[Variable, Variable]()
    def eq1(t1: Term, t2: Term): Boolean = (t1, t2) match {
      case (v1: Variable, v2: Variable) => (map1to2.get(v1), map2to1.get(v2)) match {
        case (Some(v3), Some(v4)) => v2 == v3 && v1 == v4
        case (None, None) => map1to2(v1) = v2; map2to1(v2) = v1; true
        case _ => false
      }
      case (t1, t2) => t1.productPrefix == t2.productPrefix && 
        t1.name == t2.name && ((t1.args zip t2.args) forall {case (a, b) => eq1(a, b)})
    }
    eq1(term1, term2)
  }
  
  def renaming(term1: Term, term2: Term) = {
    assume(equivalent(term1, term2))
    var map1to2 = scala.collection.mutable.Map[Variable, Variable]()
    def proceed(t1: Term, t2: Term): Unit = (t1, t2) match {
      case (v1: Variable, v2: Variable) => map1to2(v1) = v2
      case (Constructor(_, args1), Constructor(_, args2)) =>
        ((args1 zip args2) map (args => proceed(args._1, args._2)))
      case (FCall(_, args1), FCall(_, args2)) =>
        ((args1 zip args2) map (args => proceed(args._1, args._2)))
      case (GCall(_, arg01 :: args1), GCall(_, arg02 :: args2)) =>
        proceed(arg01, arg02)
        ((args1 zip args2) map (args => proceed(args._1, args._2)))
      case _ => 
    }
    proceed(term1, term2)
    map1to2
  }
  
  // tests whether t2 is instance of t1
  // t1 <~ t2 => t1{theta} = t2
  def instanceOf(t1: Term, t2: Term): Boolean = equivalent(msg(t1, t2).term, t1)
  def incommensurable(t1: Term, t2: Term) = equivalent(msg(t1, t2).term, Variable("$"))
  
  // Definition 14
  def isTrivial(expr: Expression): Boolean = expr match {
    case l: LetExpression => !l.bindings.isEmpty
    case c: Constructor => true
    case v: Variable => true
    case _ => false
  }
  
  // Definition 15
  private def b(t: Term): Int = t match {
    case g: GCall => b(g.args.head)
    case f: FCall => 0
    case c: Constructor => 0
    case v: Variable => 1 
  }
  
  private var counter = 0
  def nextVar(): Variable = {
    counter += 1
    Variable("$" + counter)
  }

}
