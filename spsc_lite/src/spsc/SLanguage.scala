package spsc

abstract class Term

case class Var(name: String) extends Term {
  override def toString = name
}

object TKind extends Enumeration {
  val Ctr, FCall, GCall = Value
}

case class CFG(kind: TKind.Value, name: String, args: List[Term]) extends Term {
  def replaceArgs(newArgs: List[Term]) = CFG(kind, name, newArgs)
  override def toString = name + args.mkString("(", ", " ,")")
}

object Ctr extends ((String, List[Term]) => CFG) {
  def apply(name: String, args: List[Term]) = CFG(TKind.Ctr, name, args)
  def unapply(e: CFG) = if (e.kind == TKind.Ctr) Some(e.name, e.args) else None
}

object FCall extends ((String, List[Term]) => CFG) {
  def apply(name: String, args: List[Term]) = CFG(TKind.FCall, name, args)
  def unapply(e: CFG) = if (e.kind == TKind.FCall) Some(e.name, e.args) else None
}

object GCall extends ((String, List[Term]) => CFG) {
  def apply(name: String, args: List[Term]) = CFG(TKind.GCall, name, args)
  def unapply(e: CFG) = if (e.kind == TKind.GCall) Some(e.name, e.args) else None
}

case class Let(term: Term, bindings: List[(Var, Term)]) extends Term {
  override def toString = "let " + bindings.map{case (v, t) => v + "=" + t}.mkString(", ") + " in " + term
}

case class Pattern(name: String, args: List[Var]) {
  override def toString = name + args.mkString("(", ", " ,")")
}

abstract class Def {def name: String}
case class FFun(name: String, args: List[Var], term: Term) extends Def {
  override def toString = name + args.mkString("(", ", " ,")") + " = " + term + ";"
}

case class GFun(name: String, p: Pattern, args: List[Var], term: Term) extends Def {
  override def toString = name + (p :: args).mkString("(", ", " ,")")  + " = " + term + ";"
}

case class Program(defs: List[Def]){
  val f = (defs :\ (Map[String, FFun]())) 
    {case (x: FFun, m) => m + (x.name -> x); case (_, m) => m}
  val g = (defs :\ (Map[(String, String), GFun]())) 
    {case (x: GFun, m) => m + ((x.name, x.p.name) -> x); case (_, m) => m}
  val gs = (defs :\ Map[String, List[GFun]]().withDefaultValue(Nil)) 
    {case (x: GFun, m) => m + (x.name -> (x :: m(x.name))); case (_, m) => m}
  override def toString = defs.mkString("\n")
}