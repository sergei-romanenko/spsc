package spsc

import TermAlgebra._

// Homeomorphic embedding checker
object HE {  
  
  def he(term1: ATerm, term2: ATerm): Boolean = 
    heByVar(term1, term2) || heByDiving(term1, term2) || heByCoupling(term1, term2)
  
  def heByVar(term1: ATerm, term2: ATerm): Boolean = (term1, term2) match {
    case (AVar(_), AVar(_)) => true
    case _ => false
  }
  
  def heByDiving(term1: ATerm, term2: ATerm): Boolean = term2 match {
    case (ASym(_, args)) => args exists (he(term1, _))
    case _ => false
  }
  
  def heByCoupling(term1: ATerm, term2: ATerm): Boolean = (term1, term2) match {
    case (ASym(name1, args1), ASym(name2, args2)) if name1 == name2 && args1.size == args2.size => 
      (args1 zip args2) forall (args => he(args._1, args._2)) 
    case _ => false
  }
}