package spsc.tests

import org.junit.Test
import org.junit.Assert._
import Algebra._

class Algebra_Tests {

  def toStringTest(expected : String, e : Term) : Unit = {
    assertEquals(expected, e.toString())
  }

  @Test def test001_toString() : Unit = {
    toStringTest("x", Var("x"))
    toStringTest("A(x, y)", Ctr("A", List(Var("x"), Var("y"))))
    toStringTest("fX(x, y)", FCall("fX", List(Var("x"), Var("y"))))
    toStringTest("gX(x, y)", GCall("gX", List(Var("x"), Var("y"))))
    toStringTest("let x=y in y", Let(Var("y"), List((Var("x"), Var("y")))))
    toStringTest(
        "let x=y,a=b in y",
        Let(Var("y"), List((Var("x"), Var("y")), (Var("a"), Var("b")))))
  }

  @Test def test002Subst() : Unit = {
    val e1 = SParsers.parseTerm("E1()")
    val e2 = SParsers.parseTerm("E2()")
    val e = SParsers.parseTerm("Cons(x1,Cons(x2,Cons(x3,Nil())))")
    val subst = Map(Var("x1")->e1, Var("x2")->e2)
    assertEquals("Cons(E1(), Cons(E2(), Cons(x3, Nil())))",
                 applySubst(subst, e).toString())
  }

  @Test def test003Vars() : Unit = {
    val e = SParsers.parseTerm("A(x,B(y,z),a)")
    assertEquals(List(Var("x"), Var("y"), Var("z"), Var("a")), vars(e))
  }

  def substToString(subst : Map[Var, Term]) : String = {
    if( subst == null)
      null
    else {
      var acc = ""
      for((v, e) <- subst)
        acc += v.toString() + "->" + e.toString() + ";"
        acc.mkString("")
    }
  }
  
  def matchOK(pat : String, exp : String, expected : String) : Unit = {
    val subst = matchAgainst(SParsers.parseTerm(pat), SParsers.parseTerm(exp))
    assertEquals(expected, substToString(subst))
  }

  def matchNo(pat : String, exp : String) : Unit = {
    val subst = matchAgainst(SParsers.parseTerm(pat), SParsers.parseTerm(exp))
    assertEquals(null, substToString(subst))
  }

  @Test def test101MatchV_E() : Unit = {
    matchOK("x", "S(Z())", "x->S(Z());")
  }

  @Test def test102MatchC_V() : Unit = {
    matchNo("Z()", "x")
  }

  @Test def test103MatchC_C() : Unit = {
    matchOK("C(x,y)", "C(A(),B())", "x->A();y->B();")
  }

  @Test def test104MatchC1_C2() : Unit = {
    matchNo("C(x,y)", "D(A(),B())")
  }

  @Test def test105MatchC_F() : Unit = {
    matchNo("C(x,y)", "f(A,B)")
  }

  @Test def test106MatchX_X_Eq() : Unit = {
    matchOK("C(x,x)", "C(A(),A())", "x->A();")
  }

  @Test def test107Match_X_XY() : Unit = {
    matchNo("C(x,y)", "C(A(),B(),C())")
  }

  @Test def testMatch_XY_X() : Unit = {
    matchNo("C(x,y,z)", "C(A(),B())")
  }

  def equivYes(e1 : String, e2 : String) : Unit = {
    assertTrue(equiv(SParsers.parseTerm(e1), SParsers.parseTerm(e2)))
  }

  @Test def test201EquivYes() : Unit = {
    equivYes("gA(fB(x,y),C)", "gA(fB(a,b),C)")
  }

  def equivNo(e1 : String, e2 : String) : Unit = {
    assertFalse(equiv(SParsers.parseTerm(e1), SParsers.parseTerm(e2)))
  }

  @Test def test301EquivNo() : Unit = {
    equivNo("gA(fB(x,y),x)", "gA(fB(a,a),b)")
  }
}
