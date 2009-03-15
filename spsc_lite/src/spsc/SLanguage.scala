package spsc

sealed abstract class Term
case class Variable(name: String) extends Term
case class Constructor(name: String, args: List[Term]) extends Term
abstract class Call(val f: String) extends Term
case class FCall(name: String, args: List[Term]) extends Call(name)
case class GCall(name: String, args: List[Term]) extends Call(name)
case class LetExpression(term: Term, bindings: List[(Variable, Term)]) extends Term
case class Pattern(name: String, args: List[Variable])

sealed abstract class Definition {def name: String}
case class FFunction(name: String, args: List[Variable], term: Term) extends Definition
case class GFunction(name: String, arg0: Pattern, args: List[Variable], term: Term) extends Definition

case class Program(defs : List[Definition]){
  val f = (defs :\ (Map[String, FFunction]())) 
    {case (x@FFunction(name, _, _), map) => map + (name -> x); case (_, map) => map}
  val g = (defs :\ (Map[(String, String), GFunction]())) 
    {case (x@GFunction(name, p, _, _), map) => map + ((name, p.name) -> x); case (_, map) => map}
  val gs = (defs :\ Map[String, List[GFunction]]()) 
    {case (g_ : GFunction, map) => map.update(g_.name, g_ :: map.getOrElse(g_.name, Nil)); case (_, map) => map}
  override def toString = defs.mkString("\n")
}