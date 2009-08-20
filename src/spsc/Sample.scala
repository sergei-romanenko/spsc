package spsc
object Sample {
  
  val target1  = 
    "gApp(gApp(x, y), z)"
  val program1 = 
    """
    gApp(Nil(), vs) = vs; 
    gApp(Cons(u, us), vs) = Cons(u, gApp(us, vs));
    """
  
  val target2 = 
    "fMain(x, y, z)"
  val program2 = 
    """
    fMain(x, y, z) = gAppend(gAppend(x, y), z);
    gAppend(Nil(), vs1) = vs1;
    gAppend(Cons(u, us), vs) = Cons(u, gAppend(us, vs));
    """
  
  val target3 =
    "gRev(x)"
  val program3 =
    """
    gApp(Nil(), vs) = vs;
    gApp(Cons(u, us), vs) = Cons(u, gApp(us, vs));
    gRev(Nil()) = Nil();
    gRev(Cons(x, xs))=gApp(gRev(xs), Cons(x, Nil()));
    """
  
  val target4 =
    "gAddAcc(a, b)"
  val program4 = 
    """
    gAddAcc(Z(), y) = y;
    gAddAcc(S(x), y) = gAddAcc(x, S(y));
    """
  
  val target5 = 
    "f1(z)"
  val program5 = 
    """
    f1(x) = f2(x);
    f2(x) = g1(x);
    f3(x) = f1(x);
    g1(A(a)) = f1(a);
    g1(B(b)) = f1(b);
    """
  
  val target6 =
    "fEqxx(x)"
  val program6 =
    """
    gEq(Z(), y) = gEqZ(y);
    gEq(S(x), y) = gEqS(y, x);

    gEqZ(Z()) = True();
    gEqZ(S(x)) = False();

    gEqS(Z(), x) = False();
    gEqS(S(y), x) = gEq(x, y);

    fEqxx(x) = gEq(x, x);
    """
  
  def main(args : Array[String]) : Unit = {
    runBaseSuperCompiler(target1, program1)
    runSuperCompiler(target1, program1)
    
    runBaseSuperCompiler(target2, program2)
    runSuperCompiler(target2, program2)
    
    // BaseSuperCompiler will not terminate for input 3
    // cause generalization is needed
    runSuperCompiler(target3, program3)
    
    runBaseSuperCompiler(target4, program4)
    runSuperCompiler(target4, program4)
    
    runBaseSuperCompiler(target5, program5)
    runSuperCompiler(target5, program5)
    
    runBaseSuperCompiler(target6, program6)
    runSuperCompiler(target6, program6)
  }
  
  def runSuperCompiler(targetText: String, programText: String) = {
    val program = SParsers.parseProg(programText)
    val target = SParsers.parseTerm(targetText)
    val sc = new SuperCompiler(program)
    val pt = sc.buildProcessTree(target)
    val (resTerm, resProgram) = new ResidualProgramGenerator(pt).result
    println("** runSuperCompiler **"); println();
    println(target); println(); println(program);
    println(); println();
    println(resTerm); println(); println(resProgram);
    println("-------")
  }
  
  def runBaseSuperCompiler(targetText: String, programText: String) = {
    val program = SParsers.parseProg(programText)
    val target = SParsers.parseTerm(targetText)
    val sc = new BaseSuperCompiler(program)
    val pt = sc.buildProcessTree(target)
    val (resTerm, resProgram) = new ResidualProgramGenerator(pt).result
    println("** runBaseSuperCompiler **"); println();
    println(target); println(); println(program);
    println(); println();
    println(resTerm); println(); println(resProgram);
    println("-------")
  }
}