package spsc
object sample {
  
  def main(args : Array[String]) : Unit = {
    m0()
  }
  
  def m0() : Unit = {
    val programText = 
    """
    fApp3(x, y, z) = gApp(gApp(x, y), z);
    gApp(Nil(), vs1) = vs1;
    gApp(Cons(u, us), vs) = Cons(u, gApp(us, vs));
    """
    val program = SParsers.parseProgram(programText)
    val sc = new SuperCompiler(program)
    val pt = sc.buildProcessTree(SParsers.parseTerm("fApp3(x, y, z)"))
    val residualProgram = new ResidualProgramGenerator(pt).result
    println(residualProgram)
  }
}
