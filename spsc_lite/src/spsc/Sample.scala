package spsc
import scala.util.parsing.input.CharArrayReader
object Sample {
  def main(args : Array[String]) : Unit = {
    val programText = 
    """
    fGoal(x, y, z) = gAppend(gAppend(x, y), z);
    gAppend(Nil(), vs1) = vs1;
    gAppend(Cons(u, us), vs) = Cons(u, gAppend(us, vs));
    
    f(a, b) = g1(a, b);
    g1(Nil(), v) = Nil();
    g2(Cons(x, y)) = Nil();
    """
    val inputText = "fGoal(x1, y1, z1)"  //"fGoal(a, b, c)"
    val inputTerm = SLanguageParsers.parseTerm(inputText)
    val program = SLanguageParsers.parseProgram(programText)

    
    //val f: FFun = program.f("fApp2") // f-функция
    //val g: GFun = program.g("gApp", "Nil") // g-функция, соответствующей образцу
    //val gs: List[GFun] = program.gs("gApp") // список g-функций
    
    val sc = new SuperCompiler(program)
    val pt = sc.buildProcessTree(inputTerm)
    val residualProgram = new ResidualProgramGenerator(pt).generateProgram()
    
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
    val inputText = "f1(z)"
    val program = SLanguageParsers.parseProgram(programText)
    val inputTerm = SLanguageParsers.parseTerm(inputText)
    
    val sc = new SuperCompiler(program)
    val pt = sc.buildProcessTree(inputTerm)
    val residualProgram = new ResidualProgramGenerator(pt).generateProgram()
    
    println(program)
    println()
    println(residualProgram)
  }
}
