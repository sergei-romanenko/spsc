package spsc
import scala.util.parsing.input.CharArrayReader
object Sample {
  def main(args : Array[String]) : Unit = {
    val programText = 
    """
    fGoal(x, y, z) = gAppend(gAppend(x, y), z);
    gAppend(Nil(), vs1) = vs1;
    gAppend(Cons(u, us), vs) = Cons(u, gAppend(us, vs));
    
    f1(z,x)= fHead(x, y);
    fHead(a, b) = g1(a, b);
    g1(Nil(), v) = v;
    g1(Cons(x, y), v) = fHead(y, v);
    """
    val inputText = "f1(a1, a2)"  //"fGoal(a, b, c)"
    val inputTerm = SParsers.parseTerm(inputText)
    val program = SParsers.parseProgram(programText)

    
    //val f: FFun = program.f("fApp2") // f-функция
    //val g: GFun = program.g("gApp", "Nil") // g-функция, соответствующей образцу
    //val gs: List[GFun] = program.gs("gApp") // список g-функций
    
    val sc = new SuperCompiler(program)
    val pt = sc.buildProcessTree(inputTerm)
    val residualProgram = new ResidualProgramGenerator(pt).residualProgram
    
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
