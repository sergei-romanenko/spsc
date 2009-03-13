package spsc
  
abstract class Expression
sealed abstract class Term extends Expression
case class Variable(name: String) extends Term {
  override def toString() = name
}
case class Constructor(name: String, args: List[Term]) extends Term {
  override def toString = name + args.mkString("(", ", " ,")")
}
sealed abstract class Call extends Term
case class FCall(name: String, args: List[Term]) extends Call {
  override def toString = name + args.mkString("(", ", " ,")")
}
case class GCall(name: String, arg0: Term, args: List[Term]) extends Call {
  override def toString = name + (arg0 :: args).mkString("(", ", " ,")")
} 
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
case class Program(defs : List[Definition]){
  var f   = Map[String, FFunction]()
  var g   = Map[(String, String), GFunction]()
  var gs = Map[String, List[GFunction]]()
  for (d <- defs) d match {
    case f_ @ FFunction(name, _, _) => f += (name -> f_)
    case g_ @ GFunction(name, arg0, _, _) => 
      g += ((name, arg0.name) -> g_) 
      gs = gs.update(name, g_ :: gs.getOrElse(name, Nil))
  }
  override def toString = defs.mkString("\n")
}
case class LetExpression(term: Term, bindings: List[Pair[Variable, Term]]) extends Expression {
  override def toString = "let " + bindings.toList.map(kv => kv._1 + "=" + kv._2).mkString("", ", ", "") + " in " + term
}