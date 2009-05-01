package spsc
object Sample {
  
  def main(args : Array[String]) : Unit = {
    val tasks = List((
    """
    fMain(x, y, z) = gAppend(gAppend(x, y), z);
    gAppend(Nil(), vs1) = vs1;
    gAppend(Cons(u, us), vs) = Cons(u, gAppend(us, vs));
    """,
    "fMain(x, y, z)"
    ),(
    """
    fMain(x) = gRev(x);
    gApp(Nil(), vs1) = vs1;
    gApp(Cons(u, us), vs) = Cons(u, gApp(us, vs));
    gRev(Nil()) = Nil();
    gRev(Cons(b, bs)) = gApp( gRev(bs), Cons(b, Nil()));
    """, 
    "fMain(x)"
    ),(
    """
    fMain(x, y, z) = gAppend(gAppend(x, y), z);
    gAppend(Nil(), vs1) = vs1;
    gAppend(Cons(u, us), vs) = Cons(u, gAppend(us, vs));
    """, 
    "fMain(x, y, x)"
    ),(
    """
    f1(x) = f2(x);
    f2(x) = g1(x);
    f3(x) = f1(x);
    g1(A(a)) = f1(a);
    g1(B(b)) = f1(b);
    """,
    "f1(z)"
    ),(
    """
    gEq(Z(), y) = gEqZ(y);
    gEq(S(x), y) = gEqS(y, x);

    gEqZ(Z()) = True();
    gEqZ(S(x)) = False();

    gEqS(Z(), x) = False();
    gEqS(S(y), x) = gEq(x, y);

    fEqxx(x) = gEq(x, x);
    """, 
    "fEqxx(x)"))
    tasks map {case (p, t) => runSample(p, t)}
  }
  
  def runSample(programText: String, termText: String) = {
    val program = SParsers.parseProgram(programText)
    val sc = new SuperCompiler(program)
    val pt = sc.buildProcessTree(SParsers.parseTerm(termText))
    val result = new ResidualProgramGenerator(pt).result
    println("---------")
    println(program)
    println()
    println(result._1)
    println(result._2)
  }
}
