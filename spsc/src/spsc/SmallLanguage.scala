package spsc

object SmallLanguage {
  
  abstract class Expression
  // The base class for terms.
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
    override def toString = name + "(" + arg0 + (args match {case Nil => ""; case _ => ", " + args.mkString(", ")})  + ")"
  }
  
  // Patterns are used in g-functions.
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
    private var gpMap = scala.collection.mutable.Map[(String, String), GFunction]()
    private var gMap = scala.collection.mutable.Map[String, List[GFunction]]()
    
    for (d <- definitions) d match {
      case f @ FFunction(name, _, _) => 
        fMap += (name -> f)
      case g @ GFunction(name, arg0, _, _) => {
        gpMap += ((name, arg0.name) -> g)
        gMap.get(name) match {
          case None => gMap(name) = List(g)
          case Some(l) => gMap(name) = g :: l
        }
      }
    }
    
    def getFFunction(name: String) = fMap.get(name) match {
      case Some(f) => f
      case None => throw new IllegalArgumentException("f-function " + name + " is undefined")
    }

    def getGFunction(name: String, cname: String) = gpMap.get((name, cname)) match {
      case Some(g) => g
      case None => throw new IllegalArgumentException("g-function " + name + " with constructor " + cname + " is undefined")
    }
    
    def getGFunctions(name: String) = gMap.get(name) match {
      case Some(l) => l
      case None => throw new IllegalArgumentException("g-function " + name + " is undefined")
    }
  }
  
  // An auxilary entity used for supercompilation.
  case class LetExpression(term: Term, bindings: Map[Variable, Term]) extends Expression {
    override def toString = "let " + bindings.toList.map(kv => kv._1 + "=" + kv._2).mkString("", ", ", "") + " in " + term
  }  
  
}
