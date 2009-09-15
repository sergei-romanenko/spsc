package spsc.tests

import org.junit.Test
import org.junit.Assert._
import Algebra._
import MSG._

class MSG_Tests {

  def msgOK(e1 : String, e2 : String, expected : String) : Unit = {
      resetVarGen()
      val gen = msg(SParsers.parseTerm(e1), SParsers.parseTerm(e2))
      assertEquals(expected, gen.toString)
  }

  @Test def test101CommonFunctor() : Unit =
  {
      msgOK("A(a1,C(a2,a3))", "A(b1,C(b2,b3))",
          "Gen(A(v2, C(v4, v5)),Map(v2 -> a1, v4 -> a2, v5 -> a3),Map(v2 -> b1, v4 -> b2, v5 -> b3))")
  }

  @Test def test102MergeSubexp1() : Unit = {
      msgOK(
          "f(a1,a2,a1)",
          "f(b1,b2,b1)",
          "Gen(f(v4, v3, v4),Map(v3 -> a2, v4 -> a1),Map(v3 -> b2, v4 -> b1))")
  }

  @Test def test103MergeSubexp2() : Unit = {
      msgOK("f(a,a)", "f(b,S(b))",
          "Gen(f(v2, v3),Map(v2 -> a, v3 -> a),Map(v2 -> b, v3 -> S(b)))")
  }
}
