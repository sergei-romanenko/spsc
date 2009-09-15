package spsc.tests

import org.junit.Test
import org.junit.Assert._
import Algebra._
import HE._

class HE_Tests {

  def heTrue(input1 : String, input2 : String) : Unit = {
    val e1 = SParsers.parseTerm(input1)
    val e2 = SParsers.parseTerm(input2)
    assertTrue(he(e1, e2))
  }

  def heFalse(input1 : String, input2 : String) : Unit = {
    val e1 = SParsers.parseTerm(input1)
    val e2 = SParsers.parseTerm(input2)
    assertFalse(he(e1, e2))
  }

  def varAttackTrue(input : String) : Unit = {
    val e = SParsers.parseTerm(input)
    assertTrue(aVarIsUnderAttack(e))
  }

  def varAttackFalse(input : String) : Unit = {
    val e = SParsers.parseTerm(input)
    assertFalse(aVarIsUnderAttack(e))
  }

  @Test def test101VarAttack() : Unit = {
    varAttackTrue("x")
    varAttackFalse("A()")
    varAttackFalse("f(x)")
    varAttackTrue("g(x,y)")
    varAttackTrue("g1(g2(x))")
    varAttackFalse("g(A())")
    varAttackFalse("g(f(x))")
  }

  @Test def test201VV() : Unit = {
    heTrue("v1", "v2")
  }

  @Test def test202VF() : Unit = {
    heTrue("v1", "F(v2)")
  }

  @Test def test203FV() : Unit = {
    heFalse("F(v2)", "v1")
  }

  @Test def test204Diving() : Unit = {
    heTrue("F(v1)", "G(v0,F(H(v2)))")
  }

  @Test def test205Coupling1() : Unit = {
    heTrue("F(v1,G(v2))", "F(H(w1),G(w2))")
  }

  @Test def test206Coupling2() : Unit = {
    heFalse("f(v1)", "g(w1)")
  }

}
