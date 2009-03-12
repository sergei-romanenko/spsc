package spsc
  
case class Program(defs : List[Definition]){
  
  var f   = Map[String, FFunction]()
  var g   = Map[(String, String), GFunction]()
  var gs = Map[String, List[GFunction]]()
  
  for (d <- defs) d match {
    case f_ @ FFunction(name, _, _) => f += (name -> f_)
    case g_ @ GFunction(name, arg0, _, _) => {
      g += ((name, arg0.name) -> g_)
      gs = gs.update(name, g_ :: gs.getOrElse(name, Nil))
    }
  }
  
  override def toString = defs.mkString("\n")
}
  
// An auxilary entity used for supercompilation.
case class LetExpression(term: Term, bindings: List[Pair[Variable, Term]]) extends Expression {
  override def toString = "let " + bindings.toList.map(kv => kv._1 + "=" + kv._2).mkString("", ", ", "") + " in " + term
}