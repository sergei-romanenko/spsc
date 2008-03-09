package spsc;

import org.junit.Test
import org.junit.Assert._
import SmallLanguage._
import TestUtils.runTest

class InterpreterTest {
  @Test def simpleAppend(): Unit =
  {
    val program = 
    """
    |test1() = append(Nil, Nil);
    |test2() = append(Cons(A1, Cons(A2, Nil)), Cons(A3, Cons(A4, Nil)));
    |test3() = reverse(Cons(A1, Cons(A2, Cons(A3, Nil))));
    |append(Nil, vs) = vs;
    |append(Cons(u, us), vs) = Cons(u, append(us, vs));
    |reverse(Nil) = Nil;
    |reverse(Cons(x, xs)) = append(reverse(xs), Cons(x, Nil));
    """

    runTest(program, "test1()", "Nil")
    runTest(program, "test2()", "Cons(A1, Cons(A2, Cons(A3, Cons(A4, Nil))))")
    runTest(program, "test3()", "Cons(A3, Cons(A2, Cons(A1, Nil)))")
  }

}
