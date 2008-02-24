package spsc;

import scala.util.parsing.input.CharArrayReader

import org.junit.Test
import org.junit.Assert._
import SmallLanguage._


class InterpreterTest {
  @Test def simpleAppend(): Unit ={
    val programText = 
    """
    |test1() = a(Nil, Nil);
    |test2() = a(Cons(A, Cons(A, Nil)), Cons(A, Cons(A, Nil)));
    |a(Nil, vs) = vs;
    |a(Cons(u, us), vs) = Cons(u, a(us, vs));
    """
    val expRes1Text = "Nil"
    val expRes2Text = "Cons(A, Cons(A, Cons(A, Cons(A, Nil))))"
        
    val in = new CharArrayReader(programText.stripMargin.toCharArray)
    val parseResult = SmallLanguage.parseProgram(in)
    println(parseResult)
    val program = parseResult.get    
    val interpreter = new Interpreter(program)
    
    val res1 = interpreter.eval(Call("test1", Nil, CallType.F))
    println(res1)
    val exp1 = SmallLanguage.parseTerm(new CharArrayReader(expRes1Text.toCharArray)).get
    println(exp1)
    assertEquals(exp1, res1)
    
    val res2 = interpreter.eval(Call("test2", Nil, CallType.F))
    println(res2)
    val exp2 = SmallLanguage.parseTerm(new CharArrayReader(expRes2Text.toCharArray)).get
    println(exp2)
    assertEquals(exp2, res2)
  }

}
