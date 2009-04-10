package spsc

abstract class Term 
case class Var(name: String) extends Term {
  override def toString = name
}
case class Ctr(name: String, args: List[Term]) extends Term {
  override def toString = name + args.mkString("(", ", " ,")")
}
abstract class Call extends Term {def name: String}
case class FCall(name: String, args: List[Term]) extends Call {
  override def toString = name + args.mkString("(", ", " ,")")
}
case class GCall(name: String, args: List[Term]) extends Call {
  override def toString = name + args.mkString("(", ", " ,")")
}
case class Let(term: Term, bindings: List[(Var, Term)]) extends Term
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

object Algebra {
  def sub(term: Term, map: Map[Var, Term]): Term = term match {
    case v: Var => map.getOrElse(v, v)
    case Ctr(n, vs)  => Ctr(n,  vs.map(sub(_, map)))
    case FCall(n, vs) => FCall(n, vs.map(sub(_, map)))
    case GCall(n, vs) => GCall(n, vs.map(sub(_, map)))
  }
  def equiv(t1: Term, t2: Term): Boolean = inst(t1, t2) && inst(t2, t1)
  def inst(t1: Term, t2: Term): Boolean = findSub(t1, t2) != null
  def findSub(t1: Term, t2: Term): Map[Var, Term] = {
    val map = scala.collection.mutable.Map[Var, Term]()
    def walk(t1: Term, t2: Term): Boolean = (t1, t2) match {
      case (v1: Var, _) => map.getOrElse(v1, t2) == (map+(v1 -> t2))(v1)
      case (Ctr(n1, xs),  Ctr(n2, ys))  => n1 == n2 && List.forall2(xs, ys)(walk)
      case (FCall(n1, xs), FCall(n2, ys)) => n1 == n2 && List.forall2(xs, ys)(walk)
      case (GCall(n1, xs), GCall(n2, ys)) => n1 == n2 && List.forall2(xs, ys)(walk)
      case _ => false
    }
    if (walk(t1, t2)) Map(map.toList:_*).filter{case (k, v) => k != v} else null
  }
  def vars(t: Term): List[Var] = t match {
    case v: Var   => (List(v))
    case c: Ctr  => (List[Var]()  /: c.args) {case (l, a) => l union vars(a)}
    case f: FCall => (List[Var]() /: f.args) {case (l, a) => l union vars(a)}
    case g: GCall => (List[Var]() /: g.args) {case (l, a) => l union vars(a)}
  }
}

case class Branch(v: Var, pat: Pattern)
class Edge(val parent: Node, var child: Node, val branch: Branch)
class Node(val expr: Term, val in: Edge, var outs: List[Edge]) {
  var fnode: Node = null
  def ancestors(): List[Node] = if (in == null) Nil else in.parent :: in.parent.ancestors
  def leafs(): List[Node] = if (outs.isEmpty) List(this) else List.flatten(children map {_.leafs})
  def children : List[Node] = outs map {_.child}
  def isProcessed: Boolean = expr match {
    case Ctr(_, Nil) => true
    case v: Var => true
    case _ => fnode != null
  }
}
class Tree(var root: Node) {
  def leafs = root.leafs
  def replace(node: Node, exp: Term) =  node.in.child = new Node(exp, node.in, Nil)
  def addChildren(node: Node, children: List[(Term, Branch)]) =
    node.outs = for ((term, b) <- children) yield {
      val edge = new Edge(node, null, b)
      edge.child = new Node(term, edge, Nil)
      edge
    }
}

import Algebra._
class SuperCompiler(p: Program){
  def driveExp(expr: Term): List[(Term, Branch)] = expr match {
    case gCall @ GCall(name, (v : Var) :: args) =>
      for (g <- p.gs(name); val pat = freshPat(g.p); val ctr = Ctr(pat.name, pat.args))
        yield (driveExp(sub(gCall, Map(v -> ctr)))(0)._1, Branch(v, pat))
    case Ctr(name, args) => args.map((_,null))
    case FCall(name, args)  => List((sub(p.f(name).term, Map(p.f(name).args.zip(args): _*)), null))
    case GCall(name, Ctr(cname, cargs) :: args) =>
      val g = p.g(name, cname)  
      List((sub(g.term, Map((g.p.args:::g.args) zip (cargs ::: args): _*)), null))
    case GCall(name, call :: args) => driveExp(call) map {p => (GCall(name, p._1 :: args), p._2)}
    case Let(term, bs) => (term, null) :: bs.map {pair => (pair._2, null)}
  }
 
  def buildProcessTree(e: Term): Tree = {
    val t = new Tree(new Node(e, null, Nil))
    while (!t.leafs.forall{_.isProcessed}) {
      val b = t.leafs.find(!_.isProcessed).get
      if (trivial(b.expr)) {
        t.addChildren(b, driveExp(b.expr))
      } else {
        b.ancestors.find(a => inst(a.expr, b.expr)) match {
          case Some(a) => if (equiv(a.expr, b.expr)) b.fnode = a else split(t, b, a)
          case None => t.addChildren(b, driveExp(b.expr))
        }
      }
    }   
    t
  }
 
  def split(t: Tree, a: Node, b: Node) = t.replace(a, Let(a.expr, findSub(a.expr, b.expr).toList))
  def trivial(expr: Term): Boolean = expr match {case x: Call => false; case _ => true}
  private var i = 0
  private def freshPat(p: Pattern) = Pattern(p.name, p.args.map {a => i += 1; Var("v" + i)})
}

class ResidualProgramGenerator(val tree: Tree) {
  lazy val residualProgram: Program = {
    val t = walk(tree.root)
    val rootCall = tree.root.expr.asInstanceOf[FCall]
    if (sigs.get(tree.root).isEmpty) defs += FFun(rootCall.name, vars(rootCall), t)
    Program(defs.toList)
  }
 
  private def walk(n: Node): Term = if (n.fnode == null) n.expr match {
    case v: Var => v
    case Ctr(name,args) => Ctr(name, n.children.map(walk))
    case Let(_,bs) => sub(walk(n.children(0)), Map(bs.map{_._1}.zip(n.children.tail.map(walk)):_*))
    case call: Call =>
      if (n.outs(0).branch != null) {
        sigs(n) = (rename(call.name, false, "g"), vars(call))
        for (e <- n.outs) defs += GFun(sigs(n)._1, e.branch.pat, vars(call).tail, walk(e.child))
        GCall(sigs(n)._1, vars(call))
      } else if (tree.leafs.exists(_.fnode == n)) {
        sigs(n) = (rename(call.name, n == tree.root, "f"), vars(call))
        defs += FFun(sigs(n)._1, sigs(n)._2, walk(n.children(0)))
        FCall(sigs(n)._1, vars(call))
      } else walk(n.children(0))
  } else if (n.fnode.outs(0).branch == null)
    sub(FCall(sigs(n.fnode)._1, sigs(n.fnode)._2), findSub(n.fnode.expr, n.expr))
  else
    sub(GCall(sigs(n.fnode)._1, sigs(n.fnode)._2), findSub(n.fnode.expr, n.expr)) 

  private var sigs = scala.collection.mutable.Map[Node, (String, List[Var])]()
  private val defs = new scala.collection.mutable.ListBuffer[Def]
  var i = 0
  def rename(f: String, keep: Boolean, b: String) = if (keep) f else {i+=1; b + f.drop(1) + i}
}

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.{CharSequenceReader => Reader}
object SParsers extends StandardTokenParsers with ImplicitConversions {
  lexical.delimiters += ("(", ")", ",", "=", ";")
  def program = definition+
  def definition: Parser[Def] = gFun | fFun
  def term: Parser[Term] = fcall | gcall | ctr | variable
  def uid = ident ^? {case id if id.charAt(0).isUpperCase => id}
  def lid = ident ^? {case id if id.charAt(0).isLowerCase => id}
  def fid = ident ^? {case id if id.charAt(0) == 'f' => id}
  def gid = ident ^? {case id if id.charAt(0) == 'g' => id}
  def variable = lid ^^ Var
  def pattern = uid ~ ("(" ~> repsep(variable, ",") <~ ")") ^^ Pattern
  def fFun = fid ~ ("(" ~> repsep(variable, ",") <~ ")") ~ ("=" ~> term <~ ";") ^^ FFun
  def gFun = gid ~ ("(" ~> pattern) ~ ((("," ~> variable)*) <~ ")") ~ ("=" ~> term <~ ";") ^^ GFun
  def ctr = uid ~ ("(" ~> repsep(term, ",") <~ ")") ^^ Ctr
  def fcall = fid ~ ("(" ~> repsep(term, ",") <~ ")") ^^ FCall
  def gcall = gid ~ ("(" ~> repsep(term, ",") <~ ")") ^^ GCall
  def parseProgram(s: String) = Program(program(new lexical.Scanner(new Reader(s))).get)
  def parseTerm(s: String) = term(new lexical.Scanner(new Reader(s))).get
}