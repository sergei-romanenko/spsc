package spsc;

import org.junit.Test
import org.junit.Assert._
import SmallLanguage._
import TestUtils.runTest

class LazinessTest {
  @Test def lazyLists(): Unit =
  {
    val program = 
    """
    |test1() = take(S(S(Z)), from(Z));
    |from(n) = Cons(n, from(S(n)));
    |take(Z, xs) = Nil;
    |take(S(n), xs) = Cons(hd(xs), take(n, tl(xs)));
    |hd(Cons(x, xs)) = x;
    |tl(Cons(x, xs)) = xs;
    """

    runTest(program, "test1", "Cons(Z, Cons(S(Z), Nil))")
  }
}
