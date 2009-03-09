package spsc
  
case class Program(defs : List[Definition]) extends AProgram{
  
  private var fs   = Map[String, FFunction]()
  private var gs   = Map[(String, String), GFunction]()
  private var gMap = Map[String, List[GFunction]]()
  
  for (d <- defs) d match {
    case f @ FFunction(name, _, _) => fs += (name -> f)
    case g @ GFunction(name, arg0, _, _) => {
      gs += ((name, arg0.name) -> g)
      gMap = gMap.update(name, g :: gMap.getOrElse(name, Nil))
    }
  }
  
  def getFFunction(name: String) = fs(name)
  def getGFunction(name: String, cname: String) = gs((name, cname)) 
  def getGFunctions(name: String) = gMap(name)
  
  override def toString = defs.mkString("\n")
}
  
// An auxilary entity used for supercompilation.
case class LetExpression(term: Term, bindings: List[Pair[Variable, Term]]) extends Expression {
  override def toString = "let " + bindings.toList.map(kv => kv._1 + "=" + kv._2).mkString("", ", ", "") + " in " + term
}