package spsc;

import SmallLanguage._
import Tree._

class SuperCompiler(program: Program){
  
  var varIndex: Int = 0
  
  def driveNode(node: Node): Unit = {
    driveExp(node.expr) match {
      case Nil => node
      case l @ _ :: _ => {
        val edges = new scala.collection.mutable.ListBuffer[Edge]
        for (pair <- l) {
          val edge = Edge(node, null, pair._2)
          val childNode = new Node(pair._1, edge, Nil)
          edge.child = childNode
          edges += edge
        }
        node.outs = edges.toList
        for (edge <- node.outs) {
          driveNode(edge.child)
        }
      }
    }
  }
  
  def driveExp(expr: Expression): List[Pair[Term, Map[Variable, Term]]] = expr match {
    case v: Variable => Nil
    
    // C(...)
    case Constructor(name, args) => 
      args.map((_, Map()))
    
    // f(...)
    case FCall(name, args)  => {
      val originalDefinition = program.getFFunction(name)
      val renamedDefinition = renameVarsInFFunction(originalDefinition)
      val substitution: Map[Variable, Term] = 
        Map() ++  (renamedDefinition.args zip args)
      val result = performSubstitution(renamedDefinition.term, substitution)
      List((result, Map[Variable, Term]()))
    }
    
    // g(C(...), ...)
    case GCall(name, Constructor(cname, cargs), args) => {
      val originalDefinition = program.getGFunction(name, cname)
      val renamedDefinition = renameVarsInGFunction(originalDefinition)     
      val substitution: Map[Variable, Term] = 
        Map() ++  ((renamedDefinition.arg0.args zip cargs) ::: (renamedDefinition.args zip args))
      val result = performSubstitution(renamedDefinition.term, substitution)
      List((result, Map[Variable, Term]()))
    }
    
    // g(x, ...)
    case GCall(name, v : Variable, args) => 
      for (g <- program.getGFunctions(name); val sub = Map((v -> Constructor(g.arg0.name, g.arg0.args))))
        yield (performSubstitution(g.term, sub), sub)
    
    // g(f(...), ...) or g(g(...), ...)
    case GCall(name, call : Call, args) => {
      val subDrive = driveExp(call)
      subDrive.map(pair => (GCall(name, pair._1, args), pair._2))
    }
    
    case LetExpression(term, bindings) => 
      (for (pair <- bindings) yield Pair(pair._2, Map(pair))).toList
  }
  
  def renameVarsInFFunction(f: FFunction): FFunction = {
    val renaming = Map() ++ f.args.map(v => (v, renameVar(v))) 
    FFunction(f.name, f.args.map(renaming(_)), performSubstitution(f.term, renaming))    
  }
  
  def renameVarsInGFunction(g: GFunction): GFunction = {
    val renaming = Map() ++ (g.args.map(v => (v, renameVar(v))) ::: g.arg0.args.map(v => (v, renameVar(v)))) 
    GFunction(g.name, Pattern(g.arg0.name, g.arg0.args.map(renaming(_))), g.args.map(renaming(_)), performSubstitution(g.term, renaming))    
  }
  
  def renameVar(v: Variable): Variable = {
    varIndex += 1
    Variable(v.name + "_" + varIndex)
  }
  
  def performSubstitution(term: Term, map: Map[Variable, Term]): Term = term match {
    case v: Variable => 
      if (map.contains(v)) map(v) else v
    case Constructor(name, args) => 
      Constructor(name, args.map(performSubstitution(_, map)))
    case FCall(name, args) => 
      FCall(name, args.map(performSubstitution(_, map)))
    case GCall(name, arg0, args) => 
      GCall(name, performSubstitution(arg0, map), args.map(performSubstitution(_, map)))
  }  
  
}