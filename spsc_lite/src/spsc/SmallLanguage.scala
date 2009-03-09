package spsc
  
sealed abstract class Expression
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

case class Program(defs : List[Definition]) {
  
  private var fs   = Map[String, FFunction]()
  private var gs   = Map[(String, String), GFunction]()
  private var gMap = Map[String, List[GFunction]]()
  
  for (d <- defs) d match {
    case f @ FFunction(name, _, _) => fs += (name -> f)
    case g @ GFunction(name, arg0, _, _) => {
      gs += ((name, arg0.name) -> g)
      gMap(name) = g :: gMap.getOrElse(name, Nil)
    }
  }
  
  def getFFunction(name: String) = fs(name)
  def getGFunction(name: String, cname: String) = gs((name, cname)) 
  def isDefinedF(name: String) = fs.contains(name)
  def isDefinedG(name: String, cname: String) = gs.contains((name, cname))
  def getGFunctions(name: String) = gMap(name)
  
  override def toString = defs.mkString("\n")
}
  
// An auxilary entity used for supercompilation.
case class LetExpression(term: Term, bindings: List[Pair[Variable, Term]]) extends Expression {
  override def toString = "let " + bindings.toList.map(kv => kv._1 + "=" + kv._2).mkString("", ", ", "") + " in " + term
}