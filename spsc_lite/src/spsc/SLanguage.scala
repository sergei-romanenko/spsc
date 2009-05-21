package spsc

abstract class Term

case class Var(name: String) extends Term {
  override def toString = name
}

object TermKind extends Enumeration {
  val Ctr, FCall, GCall = Value
}

abstract class CFGTerm extends Term {
  def kind: TermKind.Value
  def name: String;
  def args: List[Term];
  def replaceArgs(newArgs: List[Term]): CFGTerm
  override def toString = name + args.mkString("(", ", " ,")")
}

case class Ctr(name: String, args: List[Term]) extends CFGTerm {
  override def kind = TermKind.Ctr
  override def replaceArgs(newArgs: List[Term]) : Ctr = Ctr(name, newArgs)
}

abstract class Call extends CFGTerm

case class FCall(name: String, args: List[Term]) extends Call {
  override def kind = TermKind.FCall
  override def replaceArgs(newArgs: List[Term]) : FCall = FCall(name, newArgs)
}

case class GCall(name: String, args: List[Term]) extends Call {
  override def kind = TermKind.GCall
  override def replaceArgs(newArgs: List[Term]) : GCall = GCall(name, newArgs)
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