package spsc.tests

import org.junit.Test
import org.junit.Assert._

import Algebra._

class BasicSupercompiler_Tests {

  val pAdd = "gAdd(S(x),y)=S(gAdd(x,y));gAdd(Z(),y)=y;"
  val pAddAcc = "gAddAcc(S(x),y)=gAddAcc(x,S(y));gAddAcc(Z(),y)=y;"
  
  def drStep(prog : String, e : String, expected : String) : Unit =
    drStep0(SParsers.parseProg(prog), SParsers.parseTerm(e), expected)

  def drStep0(prog : Program, e : Term, expected : String) : Unit = {
    resetVarGen()
    val scp = new BasicSupercompiler(prog)
    val branches = scp.driveExp(e)
    val branches_s = (branches map {case (exp, contr) =>
      "(" + exp.toString + "," +
        (if( contr == null ) "" else contr.toString) +
        ")";}).mkString("")
    assertEquals(expected, branches_s)
  }

  @Test def test101Ctr() : Unit =
    drStep("", "C(a,b)", "(a,)(b,)")

  @Test def test102FCall() : Unit =
    drStep("f(x)=x;", "f(A(z))", "(A(z),)")

  @Test def test103GCallCtr() : Unit =
    drStep(pAddAcc, "gAddAcc(S(S(Z())),Z())", "(gAddAcc(S(Z()),S(Z())),)")

  @Test def test104GCallVar() : Unit =
    drStep(pAddAcc, "gAddAcc(a,b)", "(b,a=Z())(gAddAcc(v1,S(b)),a=S(v1))")

  @Test def test105GCallGeneral() : Unit =
    drStep(pAddAcc, "gAddAcc(gAddAcc(a,b),c)",
    "(gAddAcc(b,c),a=Z())(gAddAcc(gAddAcc(v1,S(b)),c),a=S(v1))")

  @Test def test106Let() : Unit =
    drStep0(Program(List()),
    Let(Ctr("C", List(Var("x"), Var("y"))), List((Var("x"), Var("a")), (Var("y"), Var("b")))),
    "(C(x,y),)(a,)(b,)")
}
