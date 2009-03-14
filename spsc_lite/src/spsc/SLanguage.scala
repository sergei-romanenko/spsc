package spsc

trait Printable {
  val name : String
  val args : List[Printable]
  override def toString = name + args.mkString("(", ", " ,")")
}

abstract class Expression
sealed abstract class Term extends Expression with Printable
case class Variable(name: String) extends Term {val args = null} 
case class Constructor(name: String, args: List[Term]) extends Term
case class FCall(name: String, args: List[Term]) extends Term
case class GCall(name: String, args: List[Term]) extends Term
case class Pattern(name: String, args: List[Variable]) extends Printable

sealed abstract class Definition {
  def name: String
}
case class FFunction(name: String, args: List[Variable], term: Term) extends Definition {
  override def toString = name + args.mkString("(", ", " ,")") + " = " + term + ";"
}
case class GFunction(name: String, arg0: Pattern, args: List[Variable], term: Term) extends Definition {
  override def toString = name + (arg0 :: args).mkString("(", ", " ,")")  + " = " + term + ";"  
}
case class Program(defs : List[Definition]){
  val f = (defs :\ (Map[String, FFunction]())) 
    {case (x@FFunction(name, _, _), map) => map + (name -> x); case (_, map) => map}
  val g = (defs :\ (Map[(String, String), GFunction]())) 
    {case (x@GFunction(name, p, _, _), map) => map + ((name, p.name) -> x); case (_, map) => map}
  val gs = (defs :\ Map[String, List[GFunction]]()) 
    {case (g_ @GFunction(name, _, _, _), map) => map.update(name, g_ :: map.getOrElse(name, Nil)); case (_, map) => map}
  override def toString = defs.mkString("\n")
}
case class LetExpression(term: Term, bindings: List[Pair[Variable, Term]]) extends Expression {
  override def toString = "let " + bindings.toList.map(kv => kv._1 + "=" + kv._2).mkString("", ", ", "") + " in " + term
}