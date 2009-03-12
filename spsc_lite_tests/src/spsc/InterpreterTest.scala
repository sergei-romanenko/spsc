package spsc;

import org.junit.Test
import org.junit.Assert._
import TestUtils.runTest

class InterpreterTest {
  @Test def simpleAppend(): Unit =
  {
    val program = 
    """
    |fTest1() = gAppend(Nil(), Nil());
    |fTest2() = gAppend(Cons(A1(), Cons(A2(), Nil())), Cons(A3(), Cons(A4(), Nil())));
    |fTest3() = gReverse(Cons(A1(), Cons(A2(), Cons(A3(), Nil()))));
    |gAppend(Nil(), vs) = vs;
    |gAppend(Cons(u, us), vs) = Cons(u, gAppend(us, vs));
    |gReverse(Nil()) = Nil();
    |gReverse(Cons(x, xs)) = gAppend(gReverse(xs), Cons(x, Nil()));
    """

    runTest(program, "fTest1()", "Nil()")
    runTest(program, "fTest2()", "Cons(A1(), Cons(A2(), Cons(A3(), Cons(A4(), Nil()))))")
    runTest(program, "fTest3()", "Cons(A3(), Cons(A2(), Cons(A1(), Nil())))")
  }

}
