package spsc.tests

import org.junit.Test
import org.junit.Assert._

class SLanguage_Tests {

  def toStringTest(expected : String, e : Term) : Unit = {
    assertEquals(expected, e.toString())
  }

  @Test def test101StrVarAndCall() : Unit = {
    toStringTest("x", Var("x"))
    toStringTest("A(x,y)", Ctr("A", List(Var("x"), Var("y"))))
    toStringTest("C()", Ctr("C", List()))
    toStringTest("fX(x,y)", FCall("fX", List(Var("x"), Var("y"))))
    toStringTest("gX(x,y)", GCall("gX", List(Var("x"), Var("y"))))
  }

  @Test def test102StrLet() : Unit = {
    toStringTest("let x=y in y", Let(Var("y"), List((Var("x"), Var("y")))))
    toStringTest(
        "let x=a,y=b in x",
        Let(Var("x"), List((Var("x"), Var("a")), (Var("y"), Var("b")))))
  }

  @Test def test103StrRule() : Unit = {
    assertEquals("f(x,y)=y;",
      FRule("f", List(Var("x"), Var("y")), Var("y")).toString)
    assertEquals("g(C(x),y)=y;",
      GRule("g", Pat("C", List(Var("x"))), List(Var("y")), Var("y")).toString)
    assertEquals("g(C(),y)=y;",
      GRule("g", Pat("C", List()), List(Var("y")), Var("y")).toString)
    assertEquals("g(C())=C();",
      GRule("g", Pat("C", List()), List(), Ctr("C", List())).toString)
  }

  @Test def test104StrProgram() : Unit = {
    assertEquals("f()=A();f1()=A1();",
    Program(List(FRule("f", List(), Ctr("A",List())),
      FRule("f1", List(), Ctr("A1", List())))).toString)
    assertEquals("g(C())=A();g1(C(),x)=A();g2(C(x))=A();",
    Program(List(GRule("g", Pat("C", List()), List(), Ctr("A",List())),
      GRule("g1", Pat("C", List()), List(Var("x")), Ctr("A",List())),
      GRule("g2", Pat("C", List(Var("x"))), List(), Ctr("A",List())))).toString)
  }

  @Test def test201Eq() : Unit = {
    assertTrue(Var("x") ==  Var("x"))
    assertTrue(Var("x") !=  Var("y"))
    assertTrue(Ctr("A", List()) == Ctr("A", List()))
    assertTrue(Ctr("A", List()) != Ctr("B", List()))
    assertTrue(List() ==  List())
    assertTrue(List(Var("x")) ==  List(Var("x")))
    assertTrue(List(Var("x")) != List(Var("y")))
    assertTrue(List(Var("x")) != List(Var("x"), Var("z")))
    assertTrue(Ctr("A", List(Var("x"))) == Ctr("A", List(Var("x"))))
    assertTrue(Ctr("A", List(Var("x"))) != Ctr("A", List(Var("y"))))
  }
 
}
