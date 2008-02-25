package spsc;

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
    
    val program = TestUtils.programFromString(programText.stripMargin)    
    val interpreter = new Interpreter(program)
    
    val res1 = interpreter.eval(FCall("test1", Nil))
    println(res1)
    val exp1 = TestUtils.termFromString(expRes1Text) 
    println(exp1)
    assertEquals(exp1, res1)
    
    val res2 = interpreter.eval(FCall("test2", Nil))
    println(res2)
    val exp2 = TestUtils.termFromString(expRes2Text)
    println(exp2)
    assertEquals(exp2, res2)
  }

}
