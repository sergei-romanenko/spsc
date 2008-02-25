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
  case class Call(name: String, args: List[Term], var callType: CallType.Value) extends Term {
    override def toString = name + "_" + callType + args.mkString("(", ", ", ")")
  }
  
  // Auxilary entity used for supercompilation.
  case class LetExpression(term: Term, substitution: Map[Variable, Term]) extends Expression {
    override def toString = "let " + substitution + " in " + term
  }
  
  // Used in order to classify function calls.
  object CallType extends Enumeration(0, "F", "G", "Unknown") {
    val F, G, Unknown = Value
  }
  
  // base class for patterns
  sealed abstract class Pattern
  // really args contains only simple variable
  case class FPattern(name: String, args: List[Term]) extends Pattern {
    override def toString = name + args.mkString("(", ", ", ")")
  }
  // the first arg is constructor, the other ones are simple variables
  case class GPattern(name: String, args: List[Term]) extends Pattern {
    override def toString = name + args.mkString("(", ", ", ")")
  }
  
  case class Definition(pattern: Pattern, term: Term) {
    override def toString() = pattern + " = " + term 
  }
}

