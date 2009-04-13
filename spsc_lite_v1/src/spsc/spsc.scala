package spsc

abstract class Term{def name: String; def args: List[Term]} 
case class Var(name: String) extends Term {
  override val (toString, args) = (name, null)
}
case class Ctr(name: String, args: List[Term]) extends Term {
  override def toString = name + args.mkString("(", ", " ,")")
}
case class FCall(name: String, args: List[Term]) extends Term {
  override def toString = name + args.mkString("(", ", " ,")")
}
case class GCall(name: String, args: List[Term]) extends Term {
  override def toString = name + args.mkString("(", ", " ,")")
}
case class Let(term: Term, bs: List[(Var, Term)]) extends Term {
  val (name, args) = (null, Nil)
}
case class Pattern(name: String, args: List[Var]) {
  override def toString = name + args.mkString("(", ", " ,")")
}
case class FFun(name: String, args: List[Var], term: Term)  {
  override def toString = name + args.mkString("(", ", " ,")") + " = " + term + ";"
}
case class GFun(name: String, p: Pattern, args: List[Var], term: Term)  {
  override def toString = name + (p :: args).mkString("(", ", " ,")")  + " = " + term + ";"
}

case class Program(defs: List[Either[FFun, GFun]]){
  override def toString = defs.map(_.fold(_.toString, _.toString)).mkString("\n")
  def f(f: String) = (List.lefts(defs) find {f == _.name}).get
  def gs(g: String) = List.rights(defs) filter {g == _.name}
  def g(g: String, p: String) = (gs(g) find {p == _.p.name}).get
}

object Algebra {
  def sub(term: Term, map: Map[Var, Term]): Term = term match {
    case v: Var => map.getOrElse(v, v)
    case Ctr(n, vs)  => Ctr(n,  vs.map(sub(_, map)))
    case FCall(n, vs) => FCall(n, vs.map(sub(_, map)))
    case GCall(n, vs) => GCall(n, vs.map(sub(_, map)))
  }
  def inst(t1: Term, t2: Term) = findSub(t1, t2) != null
  def findSub(t1: Term, t2: Term) = {
    val map = scala.collection.mutable.Map[Var, Term]()
    def walk(t1: Term, t2: Term): Boolean = t1 match {
      case v: Var => map.getOrElse(v, t2) == (map+(v -> t2))(v)
      case _ => t1.getClass == t2.getClass && t1.name == t2.name && 
        List.forall2(t1.args, t2.args)(walk)
    }
    if (walk(t1, t2)) Map(map.toSeq:_*).filter{case (k, v) => k != v} else null
  }
  def vars(t: Term): List[Var] = t match {
    case v: Var => (List(v))
    case _ => (List[Var]() /: t.args) {_ union vars(_)}
  }
}

class Edge(val in: Node, var out: Node, val pat: Pattern)
case class Node(expr: Term, in: Edge, var outs: List[Edge], var fnode: Node) {
  def ancestors: List[Node] = if (in == null) Nil else in.in :: in.in.ancestors
  def leaves: List[Node] = if (outs.isEmpty) List(this) else children.flatMap(_.leaves)
  def children : List[Node] = outs map {_.out}
  def isProcessed: Boolean = expr match {
    case Ctr(_, Nil) => true
    case v: Var => true
    case _ => fnode != null
  }
}
class Tree(var root: Node) {
  def leaves = root.leaves
  def replace(node: Node, exp: Term) =  node.in.out = Node(exp, node.in, Nil, null)
  def addChildren(node: Node, children: List[(Term, Pattern)]) =
    node.outs = for ((term, p) <- children) yield {
      val edge = new Edge(node, null, p)
      edge.out = Node(term, edge, Nil, null)
      edge
    }
}

import Algebra._
class SuperCompiler(p: Program){
  def driveExp(expr: Term): List[(Term, Pattern)] = expr match {
    case gCall @ GCall(n, (v : Var) :: _) =>
      for (g <- p.gs(n); val pat = freshPat(g.p); val ctr = Ctr(pat.name, pat.args))
        yield (driveExp(sub(gCall, Map(v -> ctr)))(0)._1, pat)
    case Ctr(name, args) => args.map((_,null))
    case FCall(n, vs)  => List((sub(p.f(n).term, Map()++p.f(n).args.zip(vs)), null))
    case GCall(name, Ctr(cname, cargs) :: vs) =>
      val g = p.g(name, cname)  
      List((sub(g.term, Map((g.p.args:::g.args) zip (cargs ::: vs): _*)), null))
    case GCall(n, f :: vs) => driveExp(f) map {case (v, p) => (GCall(n, v :: vs), p)}
    case Let(term, bs) => (term, null) :: bs.map {case (_, x) => (x, null)}
  }
  
  def buildProcessTree(e: Term) = {
    val t = new Tree(Node(e, null, Nil, null))
    def split(a: Node, b: Node) = t.replace(a, Let(a.expr, findSub(a.expr, b.expr).toList))
    def step(b: Node) = if (trivial(b.expr)) t.addChildren(b, driveExp(b.expr))
       else b.ancestors.find(a => inst(a.expr, b.expr)) match {
          case Some(a) => if (inst(b.expr, a.expr)) b.fnode = a else split(b, a)
          case None => t.addChildren(b, driveExp(b.expr))
       }
    while (t.leaves.exists{!_.isProcessed}) step(t.leaves.find(!_.isProcessed).get)
    t
  }
  def trivial(expr: Term) = expr match {case _:FCall=>false;case _:GCall=>false;case _=>true}
  private var i = 0
  private def freshPat(p: Pattern) = Pattern(p.name, p.args.map {_ => i += 1; Var("v" + i)})
}
class ResidualProgramGenerator(val tree: Tree) {
  var (sigs, defs) = (Map[Node, (String, List[Var])](),List[Either[FFun, GFun]]())
  lazy val result = (walk(tree.root), Program(defs.toList))
  private def walk(n: Node): Term = if (n.fnode == null) n.expr match {
    case v: Var => v
    case Ctr(name,args) => Ctr(name, n.children.map(walk))
    case Let(_,bs) => sub(walk(n.children(0)), Map()++bs.map{_._1}.zip(n.children.tail map walk))
    case c: Term =>
      if (n.outs(0).pat != null) {
        sigs += (n -> ("g" + c.name.drop(1) + sigs.size, vars(c)))
        for (e <- n.outs) defs = Right(GFun(sigs(n)._1, e.pat, vars(c).tail, walk(e.out)))::defs
        GCall(sigs(n)._1, vars(c))
      } else if (tree.leaves.exists(_.fnode == n)) {
        sigs += (n -> ("f" + c.name.drop(1) + sigs.size, vars(c)))
        defs = Left(FFun(sigs(n)._1, sigs(n)._2, walk(n.children(0)))) :: defs
        FCall(sigs(n)._1, vars(c))
      } else walk(n.children(0))
  } else if (n.fnode.outs(0).pat == null)
       sub(Function.tupled(FCall)(sigs(n.fnode)), findSub(n.fnode.expr, n.expr))
  else sub(Function.tupled(GCall)(sigs(n.fnode)), findSub(n.fnode.expr, n.expr))
}

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.{CharSequenceReader => Reader}
object SParsers extends StandardTokenParsers with ImplicitConversions {
  lexical.delimiters += ("(", ")", ",", "=", ";")
  def defs = (fFun ^^ {Left(_)} | gFun ^^ {Right(_)})+
  def term: Parser[Term] = fcall | gcall | ctr | vrb
  def uid = ident ^? {case id if id.charAt(0).isUpperCase => id}
  def lid = ident ^? {case id if id.charAt(0).isLowerCase => id}
  def fid = ident ^? {case id if id.charAt(0) == 'f' => id}
  def gid = ident ^? {case id if id.charAt(0) == 'g' => id}
  def vrb = lid ^^ Var
  def ptr = uid ~ ("(" ~> repsep(vrb, ",") <~ ")") ^^ Pattern
  def fFun = fid ~ ("(" ~> repsep(vrb, ",") <~ ")") ~ ("=" ~> term <~ ";") ^^ FFun
  def gFun = gid ~ ("(" ~> ptr) ~ ((("," ~> vrb)*) <~ ")") ~ ("=" ~> term <~ ";") ^^ GFun
  def ctr = uid ~ ("(" ~> repsep(term, ",") <~ ")") ^^ Ctr
  def fcall = fid ~ ("(" ~> repsep(term, ",") <~ ")") ^^ FCall
  def gcall = gid ~ ("(" ~> repsep(term, ",") <~ ")") ^^ GCall
  def parseProgram(s: String) = Program(defs(new lexical.Scanner(new Reader(s))).get)
  def parseTerm(s: String) = term(new lexical.Scanner(new Reader(s))).get
}