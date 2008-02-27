package spsc

object HE {
  // TODO parser for HETerms
  sealed abstract class HETerm
  case class HEVar(name: String) extends HETerm {
    override def toString = name
  }
  case class HESym(name: String, args: List[HETerm]) extends HETerm {
    override def toString = name + args.mkString("(",",", ")") 
  }
  
  def he(term1: HETerm, term2: HETerm): Boolean = 
    heByVar(term1, term2) || heByDiving(term1, term2) || heByCoupling(term1, term2)
  
  def heByVar(term1: HETerm, term2: HETerm): Boolean = (term1, term2) match {
    case (HEVar(_), HEVar(_)) => true
    case _ => false
  }
  
  def heByDiving(term1: HETerm, term2: HETerm): Boolean = term2 match {
    case (HESym(_, args)) => args exists (he(term1, _))
    case _ => false
  }
  
  def heByCoupling(term1: HETerm, term2: HETerm): Boolean = (term1, term2) match {
    case (HESym(name1, args1), HESym(name2, args2)) if name1 == name2 && args1.size == args2.size => 
      (args1 zip args2) forall (args => he(args._1, args._2)) 
    case _ => false
  }
}