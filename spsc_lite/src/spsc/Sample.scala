package spsc
import scala.util.parsing.input.CharArrayReader
object Sample {
  def main(args : Array[String]) : Unit = {
    val programText = 
    """
    fGoal(x, y, z) = gAppend(gAppend(x,y), Nil());
    gAppend(Nil(), vs) = vs;
    gAppend(Cons(u, us), vs) = Cons(u, gAppend(us, vs));
    """
    val inputText = "fGoal(a, b, c)"
    val program = SLanguageParsers.parseProgram(programText)
    val inputTerm = SLanguageParsers.parseTerm(inputText)
    
    val sc = new SuperCompiler(program)
    val pt = sc.buildProcessTree(inputTerm)
    val residualProgram = ResidualProgramGenerator.generateResidualProgram(pt)
    
    println(program)
    println()
    println(residualProgram)
    
    println()
    
    m1()
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
    val inputText = "f3(z)"
    val program = SLanguageParsers.parseProgram(programText)
    val inputTerm = SLanguageParsers.parseTerm(inputText)
    
    val sc = new SuperCompiler(program)
    val pt = sc.buildProcessTree(inputTerm)
    val residualProgram = ResidualProgramGenerator.generateResidualProgram(pt)
    
    println(program)
    println()
    println(residualProgram)
  }
}
