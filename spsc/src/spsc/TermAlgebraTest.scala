package spsc;

import org.junit.Test
import org.junit.Assert._
import TermAlgebra._

// TODO : via parser
class TermAlgebraTest {  
  
  @Test def simpleHE() : Unit = {
    assertTrue(he(AVar("x"), AVar("y"))) //true: x<y
    assertFalse(he(AVar("x"), ASym("Y", Nil))) //false: x!<Y
    assertTrue(he(AVar("x"), ASym("Y", AVar("x")::Nil)))//true: x<Y(x) 
    assertTrue(he(ASym("Y", List()), ASym("Y", Nil)))
    
    // from Soerensen paper:
    // b < f(b)
    assertTrue(he(ASym("b", Nil), ASym("f", ASym("b", Nil) :: Nil)))
    // c(b) < c(f(b))
    assertTrue(he(ASym("c", ASym("b", Nil) :: Nil), 
                  ASym("c", ASym("f", ASym("b", Nil)::Nil) :: Nil)))                  
    // d(b, b) < d(f(b), f(b))
    assertTrue(he(ASym("d", ASym("b", Nil) :: ASym("b", Nil) :: Nil), 
                  ASym("d", ASym("f", ASym("b", Nil) :: Nil) :: ASym("f", ASym("b", Nil) :: Nil) :: Nil)))
    // f(c(b)) !< c(b)
    assertFalse(he(ASym("f", ASym("c", ASym("b", Nil)::Nil) :: Nil), 
                   ASym("c", ASym("b", Nil) :: Nil)))
    // f(c(b)) !< c(f(b))
    assertFalse(he(ASym("f", ASym("c", ASym("b", Nil)::Nil) :: Nil), 
                   ASym("c", ASym("f", ASym("b", Nil)::Nil) :: Nil)))
    // f(c(b)) !< f(f(f(b)))
    assertFalse(he(ASym("f", ASym("c", ASym("b", Nil)::Nil) :: Nil), 
                   ASym("f", ASym("f", ASym("f", ASym("b", Nil)::Nil)::Nil) :: Nil)))
  }
  
  @Test def simpleEquivalent(): Unit = {
    // B = B
    assertTrue(equivalent(ASym("B", Nil), ASym("B", Nil)))
    
    // D(x, x) = D(y, y)
    assertTrue(equivalent(ASym("D", AVar("x") :: AVar("x") :: Nil),
                          ASym("D", AVar("y") :: AVar("y") :: Nil)))

    // D(x, y) = D(y, x)
    assertTrue(equivalent(ASym("D", AVar("x") :: AVar("y") :: Nil),
                          ASym("D", AVar("y") :: AVar("x") :: Nil)))
                          
    // D(x, y) != D(z, z)
    assertFalse(equivalent(ASym("D", AVar("x") :: AVar("y") :: Nil),
                          ASym("D", AVar("z") :: AVar("z") :: Nil)))

  }
  
  @Test def simpleMSG(): Unit = {
    // A(B)^B = x
    val msg0 = msg(ASym("A", ASym("B", Nil) :: Nil), 
                   ASym("B", Nil))
    println(msg0)
    assertTrue(equivalent(msg0.term, AVar("x")))
    
    // C(B)^C(F(B)) = C(x)
    val msg1 = msg(ASym("C", ASym("B", Nil) :: Nil), 
        ASym("C", ASym("F", ASym("B", Nil)::Nil) :: Nil))
    println(msg1)
    assertTrue(equivalent(msg1.term, ASym("C", AVar("x") :: Nil)))

    // D(B, B)^D(F(B), F(B)) = D(x, x)
    val msg2 = msg(ASym("D", ASym("B", Nil) :: ASym("B", Nil) :: Nil), 
                   ASym("D", ASym("F", ASym("B", Nil) :: Nil) :: ASym("F", ASym("B", Nil) :: Nil) :: Nil))
    println(msg2)
    assertTrue(equivalent(msg2.term, ASym("D", AVar("x") :: AVar("x") :: Nil)))
  }
  
  @Test def simpleInstanceOf(): Unit = {
    // A(x) <~ A(b); A(x){x:=b)=A(b)
    assertTrue(instanceOf(ASym("A", AVar("x") :: Nil),
                          ASym("A", ASym("B", Nil) :: Nil)))
                          
    // A(x) !<~ A(b);
    assertFalse(instanceOf(ASym("A", ASym("B", Nil) :: Nil),
                           ASym("A", AVar("x") :: Nil)))
  }
}
