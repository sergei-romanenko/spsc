package spsc;

object FreeAlgebra {
  sealed abstract class ATerm
  case class AVar(name: String) extends ATerm {
    override def toString = name
  }
  case class ASym(name: String, args: List[ATerm]) extends ATerm {
    override def toString = name + args.mkString("(",",", ")") 
  }
}
