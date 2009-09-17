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

abstract class CFGObject(kind: TKind.Value) extends ((String, List[Term]) => CFG) {
  def apply(name: String, args: List[Term]) = CFG(kind, name, args)
  def unapply(e: CFG) = if (e.kind == kind) Some(e.name, e.args) else None
}
object Ctr extends CFGObject(TKind.Ctr)
object FCall extends CFGObject(TKind.FCall)
object GCall extends CFGObject(TKind.GCall)


case class Let(term: Term, bindings: List[(Var, Term)]) extends Term {
  val bindings_s = bindings map {case (v, e) => v.toString() + "=" + e.toString()}
  override def toString = "let " + bindings_s.mkString(",") + " in " + term.toString()
}

case class Pat(name: String, args: List[Var]) {
  override def toString = name + args.mkString("(", ", " ,")")
}

abstract class Def {def name: String}
case class FFun(name: String, args: List[Var], term: Term) extends Def {
  override def toString = name + args.mkString("(", ", " ,")") + " = " + term + ";"
}

case class GFun(name: String, p: Pat, args: List[Var], term: Term) extends Def {
  override def toString = name + (p :: args).mkString("(", ", " ,")")  + " = " + term + ";"
}

case class Program(defs: List[Def]){
  val f = (defs :\ (Map[String, FFun]())) 
    {case (d: FFun, m) => m + (d.name -> d); case (_, m) => m}
  val g = (defs :\ (Map[(String, String), GFun]())) 
    {case (d: GFun, m) => m + ((d.name, d.p.name) -> d); case (_, m) => m}
  val gs = (g :\ Map[String, List[GFun]]())
    {case (((n, _), d), m) => m + (n -> (d :: m.getOrElse(n, Nil)))}
  override def toString = defs.mkString("\n")
}