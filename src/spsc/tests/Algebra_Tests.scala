package spsc.tests

import org.junit.Test
import org.junit.Assert._
import Algebra._

class Algebra_Tests {

  @Test def test101TheSameFunctor() : Unit = {
    assertTrue(shellEq(Ctr("A", List()), Ctr("A", List())))
    assertTrue(shellEq(FCall("A", List()), FCall("A", List())))
    assertTrue(shellEq(GCall("A", List()), GCall("A", List())))
    assertFalse(shellEq(Ctr("A", List()), Ctr("B", List())))
    assertFalse(shellEq(Ctr("A", List()), FCall("A", List())))
    assertFalse(shellEq(Ctr("A", List()), Ctr("A", List(Var("y")))))
  }
  
  @Test def test201Subst() : Unit = {
    val e1 = SParsers.parseTerm("E1()")
    val e2 = SParsers.parseTerm("E2()")
    val e = SParsers.parseTerm("Cons(x1,Cons(x2,Cons(x3,Nil())))")
    val subst = Map(Var("x1")->e1, Var("x2")->e2)
    assertEquals("Cons(E1(),Cons(E2(),Cons(x3,Nil())))",
                 applySubst(subst, e).toString())
  }

  @Test def test302Vars() : Unit = {
    val e = SParsers.parseTerm("A(x,B(y,z),a)")
    assertEquals(List(Var("x"), Var("y"), Var("z"), Var("a")), vars(e))
    
    val e1 = SParsers.parseTerm("A(x,B(y,x),a)")
    assertEquals(List(Var("x"), Var("y"), Var("a")), vars(e1))
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

  @Test def test401MatchV_E() : Unit = {
    matchOK("x", "S(Z())", "x->S(Z());")
  }

  @Test def test402MatchC_V() : Unit = {
    matchNo("Z()", "x")
  }

  @Test def test403MatchC_C() : Unit = {
    matchOK("C(x,y)", "C(A(),B())", "x->A();y->B();")
  }

  @Test def test404MatchC1_C2() : Unit = {
    matchNo("C(x,y)", "D(A(),B())")
  }

  @Test def test405MatchC_F() : Unit = {
    matchNo("C(x,y)", "f(A,B)")
  }

  @Test def test406MatchX_X_Eq() : Unit = {
    matchOK("C(x,x)", "C(A(),A())", "x->A();")
  }

  @Test def test407Match_X_XY() : Unit = {
    matchNo("C(x,y)", "C(A(),B(),C())")
  }

  @Test def testMatch_XY_X() : Unit = {
    matchNo("C(x,y,z)", "C(A(),B())")
  }

  def equivYes(e1 : String, e2 : String) : Unit = {
    assertTrue(equiv(SParsers.parseTerm(e1), SParsers.parseTerm(e2)))
  }

  @Test def test501EquivYes() : Unit = {
    equivYes("gA(fB(x,y),C)", "gA(fB(a,b),C)")
  }

  def equivNo(e1 : String, e2 : String) : Unit = {
    assertFalse(equiv(SParsers.parseTerm(e1), SParsers.parseTerm(e2)))
  }

  @Test def test502EquivNo() : Unit = {
    equivNo("gA(fB(x,y),x)", "gA(fB(a,a),b)")
  }
}
