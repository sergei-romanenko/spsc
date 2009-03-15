package spsc

sealed abstract class Term
case class Var(name: String) extends Term
case class Cons(name: String, args: List[Term]) extends Term
abstract class Call(val f: String) extends Term
case class FCall(name: String, args: List[Term]) extends Call(name)
case class GCall(name: String, args: List[Term]) extends Call(name)
case class Let(term: Term, bindings: List[(Var, Term)]) extends Term
case class Pattern(name: String, args: List[Var])

sealed abstract class Def {def name: String}
case class FFun(name: String, args: List[Var], term: Term) extends Def
case class GFun(name: String, p: Pattern, args: List[Var], term: Term) extends Def

case class Program(defs: List[Def]){
  val f = (defs :\ (Map[String, FFun]())) 
    {case (x: FFun, m) => m + (x.name -> x); case (_, m) => m}
  val g = (defs :\ (Map[(String, String), GFun]())) 
    {case (x: GFun, m) => m + ((x.name, x.p.name) -> x); case (_, m) => m}
  val gs = (defs :\ Map[String, List[GFun]]().withDefaultValue(Nil)) 
    {case (x: GFun, m) => m + (x.name -> (x :: m(x.name))); case (_, m) => m}
  override def toString = defs.mkString("\n")
}