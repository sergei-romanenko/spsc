package spsc
import scala.util.parsing.input.CharArrayReader
object Sample {
  def main(args : Array[String]) : Unit = {
    val programText = 
    """
    fTest1() = gAppend(Nil(), Nil());
    gAppend(Nil(), vs) = vs;
    gAppend(Cons(u, us), vs) = Cons(u, gAppend(us, vs));
    """
    val defs = SmallLanguageParsers.parseProgram(new CharArrayReader(programText.toArray))
    val program = Program(defs)
    println(program)
  }
}
