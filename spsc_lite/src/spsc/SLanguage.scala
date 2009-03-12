package spsc
  
abstract class Expression
sealed abstract class Term extends Expression
// Variables start with a lower case letter and have no args.
case class Variable(name: String) extends Term {
  override def toString() = name
}
// Constructors start with an upper case letter and have optional args.
case class Constructor(name: String, args: List[Term]) extends Term {
  override def toString = name + args.mkString("(", ", " ,")")
}
// Function calls.
sealed abstract class Call extends Term
case class FCall(name: String, args: List[Term]) extends Call {
  override def toString = name + args.mkString("(", ", " ,")")
}
case class GCall(name: String, arg0: Term, args: List[Term]) extends Call {
  override def toString = name + (arg0 :: args).mkString("(", ", " ,")")
} 
// Patterns are used in g-functions.
case class Pattern(name: String, args: List[Variable]) {
  override def toString = name + args.mkString("(", ", " ,")")
}
  
sealed abstract class Definition {
  def name: String
}
case class FFunction(name: String, args: List[Variable], term: Term) extends Definition {
  override def toString = name + args.mkString("(", ", " ,")") + " = " + term + ";"
}
case class GFunction(name: String, arg0: Pattern, args: List[Variable], term: Term) extends Definition {
  override def toString = name + (arg0 :: args).mkString("(", ", " ,")")  + " = " + term + ";"  
}