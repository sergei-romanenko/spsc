package spsc;

import org.junit.Test
import org.junit.Assert._
import TestUtils.runTest

class LazinessTest {
  @Test def lazyLists(): Unit =
  {
    val program = 
    """
    |fTest1() = gTake(S(S(Z())), fFrom(Z()));
    |fFrom(n) = Cons(n, fFrom(S(n)));
    |gTake(Z(), xs) = Nil();
    |gTake(S(n), xs) = Cons(gHead(xs), gTake(n, gTail(xs)));
    |gHead(Cons(x, xs)) = x;
    |gTail(Cons(x, xs)) = xs;
    """

    runTest(program, "fTest1()", "Cons(Z(), Cons(S(Z()), Nil()))")
  }
}
