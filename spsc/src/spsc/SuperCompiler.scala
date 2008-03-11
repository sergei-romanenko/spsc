package spsc;

import SmallLanguage._
import SmallLanguageTermAlgebra._
import ProcessTree._
import Util.applySubstitution

class SuperCompiler(program: Program){
  
  def driveExp(expr: Expression): List[Pair[Term, Map[Variable, Term]]] = expr match {
    case v: Variable => Nil
    
    // C(...)
    case Constructor(name, args) => 
      args.map((_, Map()))
    
    // f(...)
    case FCall(name, args)  => {
      if (!program.isDefinedF(name)){
        return Nil
      }
      val originalDefinition = program.getFFunction(name)
      val renamedDefinition = renameVarsInFFunction(originalDefinition)
      val substitution: Map[Variable, Term] = 
        Map() ++  (renamedDefinition.args zip args)
      val result = applySubstitution(renamedDefinition.term, substitution)
      List((result, Map[Variable, Term]()))
    }
    
    // g(C(...), ...)
    case GCall(name, Constructor(cname, cargs), args) => {
      if (!program.isDefinedG(name, cname)){
        return Nil
      }
      val originalDefinition = program.getGFunction(name, cname)
      val renamedDefinition = renameVarsInGFunction(originalDefinition)     
      val substitution: Map[Variable, Term] = 
        Map() ++  ((renamedDefinition.arg0.args zip cargs) ::: (renamedDefinition.args zip args))
      val result = applySubstitution(renamedDefinition.term, substitution)
      List((result, Map[Variable, Term]()))
    }
    
    // g(x, ...)
    case gCall @ GCall(name, v : Variable, args) => 
      for (g <- program.getGFunctions(name);
        val c = Constructor(g.arg0.name, g.arg0.args.map(v => nextVar));
        val sub = Map((v -> c)))
        yield (driveExp(applySubstitution(gCall, sub)).head._1, sub)
    
    // g(f(...), ...) or g(g(...), ...)
    case GCall(name, call : Call, args) => {
      val subDrive = driveExp(call)
      subDrive.map(pair => (GCall(name, pair._1, args.map(applySubstitution(_, pair._2))), pair._2))
    }
    
    case LetExpression(term, bindings) => 
      (term, Map[Variable, Term]()) :: (for (pair <- bindings) yield Pair(pair._2, Map[Variable, Term]())).toList
  }
  
  def renameVarsInFFunction(f: FFunction): FFunction = {
    val renaming = Map() ++ f.args.map(v => (v, nextVar())) 
    FFunction(f.name, f.args.map(renaming(_)), applySubstitution(f.term, renaming))    
  }
  
  def renameVarsInGFunction(g: GFunction): GFunction = {
    val renaming = Map() ++ (g.args.map(v => (v, nextVar())) ::: g.arg0.args.map(v => (v, nextVar()))) 
    GFunction(g.name, Pattern(g.arg0.name, g.arg0.args.map(renaming(_))), g.args.map(renaming(_)), applySubstitution(g.term, renaming))    
  }
  
  // heart of supercompiler
  def buildProcessTree(e: Expression): ProcessTree = {
    val p = ProcessTree(e)
    while (!p.isClosed) {
      val beta = p.leafs.find(!_.isProcessed).get
      if (isTrivial(beta.expr) || beta.ancestors.forall(n1 => isTrivial(n1.expr) || !strictHE(n1.expr.asInstanceOf[Term], beta.expr.asInstanceOf[Term]))){
        drive(p, beta)
      } else {
        val alpha = beta.ancestors.find(n1 => !isTrivial(n1.expr) && strictHE(n1.expr.asInstanceOf[Term], beta.expr.asInstanceOf[Term])).get
        if (instanceOf(alpha.expr.asInstanceOf[Term], beta.expr.asInstanceOf[Term])){
          makeAbstraction(p, beta, alpha)
        } else if (incommensurable(alpha.expr.asInstanceOf[Term], beta.expr.asInstanceOf[Term])){
          split(p, beta)
        } else {
          makeAbstraction(p, alpha, beta)
        }
      }
    }    
    p
  }
  
  def drive(t: ProcessTree, n: Node): Unit = {
    t.addChildren(n, driveExp(n.expr))
  }
  
  def makeAbstraction(t: ProcessTree, alpha: Node, beta: Node): Unit = {
    // t(alpha) = e {x1 ...}
    val g = strongMsg(alpha.expr.asInstanceOf[Term], beta.expr.asInstanceOf[Term])
    // see Definition 7
    if (g.sub1.isEmpty){
      t.replace(alpha, g.term)
    } else {
      t.replace(alpha, LetExpression(g.term, (Map() ++ g.sub1).toList))
    }
    
  }
  
  def split(t: ProcessTree, n: Node): Unit = n.expr match {
    case c @ Constructor(name, args) => {
      val vars = args.map(a => nextVar())
      val sub = Map() ++ (vars zip args)
      t.replace(n, LetExpression(Constructor(name, vars), sub.toList))
    }
    case f @ FCall(name, args) =>
      val vars = args.map(a => nextVar())
      val sub = Map() ++ (vars zip args)
      t.replace(n, LetExpression(FCall(name, vars), sub.toList))
    case g @ GCall(name, arg0, args) =>
      val arg0Var = nextVar
      val vars = args.map(a => nextVar())
      val sub = Map() ++ ((arg0Var :: vars) zip (arg0 :: args))
      t.replace(n, LetExpression(GCall(name, arg0Var, vars), sub.toList))
    case _ => throw new IllegalArgumentException("Can not split " + n.expr)
  }
  
}