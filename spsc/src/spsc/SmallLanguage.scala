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

  // TODO: f-functions and g-functions should be kept as separate maps.
  //       the map of g-functions should return maps that map constructor
  //       names to the corresponding FFunctions.
  
  sealed case class Program(definitions : List[Definition]) {

    def getFFunction(name: String) =
      definitions.find(_ match {
        case FFunction(fname, _, _) if (fname == name) => true; 
        case _ => false}).get.asInstanceOf[FFunction]

    def getGFunction(name: String, cname: String) =
      definitions.find(_ match {
        case GFunction(gname, Pattern(gcname, _),  _, _)
          if (gname == name && gcname == cname) => true; 
        case _ => false}).get.asInstanceOf[GFunction]
  }
  
  // Auxilary entity used for supercompilation.
  case class LetExpression(term: Term, substitution: Map[Variable, Term]) extends Expression {
    override def toString = "let " + substitution + " in " + term
  }  
  
}

