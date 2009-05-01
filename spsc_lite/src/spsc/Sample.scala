package spsc
object Sample {
  
  def main(args : Array[String]) : Unit = {
    m0()
    m1()
    m2()
    m4()
  }
  
  def m0() : Unit = {
    val programText = 
    """
    fMain(x, y, z) = gAppend(gAppend(x, y), z);
    gAppend(Nil(), vs1) = vs1;
    gAppend(Cons(u, us), vs) = Cons(u, gAppend(us, vs));
    """
    val program = SParsers.parseProgram(programText)
    val sc = new SuperCompiler(program)
    val pt = sc.buildProcessTree(SParsers.parseTerm("fMain(x, y, z)"))
    val result = new ResidualProgramGenerator(pt).result
    println("---------")
    println(program)
    println()
    println(result._1)
    println(result._2)
    
  }
  
  def m4() : Unit = {
    val programText = 
    """
    fMain(x) = gRev(x);
    gApp(Nil(), vs1) = vs1;
    gApp(Cons(u, us), vs) = Cons(u, gApp(us, vs));
    gRev(Nil()) = Nil();
    gRev(Cons(b, bs)) = gApp( gRev(bs), Cons(b, Nil()));
    """
    val program = SParsers.parseProgram(programText)
    val sc = new SuperCompiler(program)
    val pt = sc.buildProcessTree(SParsers.parseTerm("fMain(x)"))
    val result = new ResidualProgramGenerator(pt).result
    println("---------")
    println(program)
    println()
    println(result._1)
    println(result._2)
  }
  
  def m3() : Unit = {
    val programText = 
    """
    fMain(x, y, z) = gAppend(gAppend(x, y), z);
    gAppend(Nil(), vs1) = vs1;
    gAppend(Cons(u, us), vs) = Cons(u, gAppend(us, vs));
    """
    val program = SParsers.parseProgram(programText)
    val sc = new SuperCompiler(program)
    val pt = sc.buildProcessTree(SParsers.parseTerm("fMain(x, y, x)"))
    val result = new ResidualProgramGenerator(pt).result
    println("---------")
    println(program)
    println()
    println(result._1)
    println(result._2)
  }
  
  def m1() : Unit = {
    val programText = 
    """
    f1(x) = f2(x);
    f2(x) = g1(x);
    f3(x) = f1(x);
    g1(A(a)) = f1(a);
    g1(B(b)) = f1(b);
    """
    val inputText = "f1(z)"
    val program = SParsers.parseProgram(programText)
    val inputTerm = SParsers.parseTerm(inputText)
    
    val sc = new SuperCompiler(program)
    val pt = sc.buildProcessTree(inputTerm)
    val result = new ResidualProgramGenerator(pt).result
    
    println("---------")
    println(program)
    println()
    println(result._1)
    println(result._2)
  }
  
  def m2() : Unit = {
    val programText = 
    """
    gEq(Z(), y) = gEqZ(y);
    gEq(S(x), y) = gEqS(y, x);

    gEqZ(Z()) = True();
    gEqZ(S(x)) = False();

    gEqS(Z(), x) = False();
    gEqS(S(y), x) = gEq(x, y);

    fEqxx(x) = gEq(x, x);
    """
    val program = SParsers.parseProgram(programText)
    val sc = new SuperCompiler(program)
    val pt = sc.buildProcessTree(SParsers.parseTerm("fEqxx(x)"))
    val result = new ResidualProgramGenerator(pt).result
    
    println("---------")
    println(program)
    println()
    println(result._1)
    println(result._2)
  }
}
