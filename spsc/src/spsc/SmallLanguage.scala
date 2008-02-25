package spsc

object SmallLanguage {
  
  abstract class Expression
  // base class for term
  sealed abstract class Term extends Expression
  // variable begins with lower case letter and has no args
  case class Variable(name: String) extends Term {
    override def toString() = name
  }
  // constructor begins with upper case letter and optionally has args
  case class Constructor(name: String, args: List[Term]) extends Term {
    override def toString = name + args.mkString("(", ", " ,")")
  }
  
  // function call
  sealed abstract class Call extends Term
  case class FCall(name: String, args: List[Term]) extends Call {
    override def toString = name + args.mkString("(", ", " ,")")
  }
  case class GCall(name: String, arg0: Term, args: List[Term]) extends Call {
    override def toString = name + "(" + arg0 + (args match {case Nil => ""; case _ => ", " + args.mkString(", ")})  + ")"
  }
  
  // pattern is used in g-function
  case class Pattern(name: String, args: List[Variable]) {
    override def toString = name + args.mkString("(", ", " ,")")
  }
  
  sealed abstract class Definition
  case class FFunction(name: String, args: List[Variable], term: Term) extends Definition {
    override def toString = name + args.mkString("(", ", " ,")") + " = " + term
  }
  case class GFunction(name: String, arg0: Pattern, args: List[Variable], term: Term) extends Definition {
    override def toString = 
      name + "(" + arg0 + (args match {case Nil => ""; case _ => ", " + args.mkString(", ")})  + ") = " + term  
  }
  
  // Auxilary entity used for supercompilation.
  case class LetExpression(term: Term, substitution: Map[Variable, Term]) extends Expression {
    override def toString = "let " + substitution + " in " + term
  }  
  
}

