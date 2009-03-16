package spsc
import scala.util.parsing.input.CharArrayReader
object Sample {
  def main(args : Array[String]) : Unit = {
    val programText = 
    """
    fMain(x, y, z) = gAppend(gAppend(x, y), z);
    gAppend(Nil(), vs1) = vs1;
    gAppend(Cons(u, us), vs) = Cons(u, gAppend(us, vs));
    """
    val program = SParsers.parseProgram(programText)
    val sc = new SuperCompiler(program)
    val pt = sc.buildProcessTree(SParsers.parseTerm("fMain(x, y, z)"))
    val residualProgram = new ResidualProgramGenerator(pt).residualProgram
    println(residualProgram)
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
    val residualProgram = new ResidualProgramGenerator(pt).residualProgram
    
    println(program)
    println()
    println(residualProgram)
  }
}
