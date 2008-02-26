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
  
  case class Program(definitions : List[Definition]) {
    
    private var fMap = scala.collection.mutable.Map[String, FFunction]()        
    private var gMap = scala.collection.mutable.Map[(String, String), GFunction]()
    
    for (d <- definitions) d match {
      case f @ FFunction(name, _, _) => fMap += (name -> f)
      case g @ GFunction(name, arg0, _, _) => gMap += ((name, arg0.name) -> g)
    }
    
    def getFFunction(name: String) = fMap.get(name) match {
      case Some(f) => f
      case None => throw new IllegalArgumentException("f-function " + name + " is undefined")
    }

    def getGFunction(name: String, cname: String) = gMap.get((name, cname)) match {
      case Some(g) => g
      case None => throw new IllegalArgumentException("g-function " + name + " with constructor " + cname + " is undefined")
    }
  }
  
  // Auxilary entity used for supercompilation.
  case class LetExpression(term: Term, bindings: Map[Variable, Term]) extends Expression {
    override def toString = "let " + bindings + " in " + term
  }  
  
}

