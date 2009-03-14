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
    val program = SLanguageParsers.parseProgram(new CharArrayReader(programText.toArray))
    val inputTerm = SLanguageParsers.parseTerm(new CharArrayReader(inputText.toArray))
    
    val sc = new SuperCompiler(program)
    val pt = sc.buildProcessTree(inputTerm)
    val residualProgram = ResidualProgramGenerator.generateResidualProgram(pt)
    
    println(program)
    println()
    println(residualProgram)
  }
}
