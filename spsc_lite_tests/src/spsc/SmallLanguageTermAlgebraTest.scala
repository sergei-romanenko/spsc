package spsc

import org.junit.Test
import org.junit.Assert._

import SmallLanguageTermAlgebra._
import TestUtils.{termFromString => __}

class SmallLanguageTermAlgebraTest {
  @Test def simpleHE() : Unit = {
    
    assertTrue(he(__("x"), __("x"))) //true: x<y
    assertFalse(he(__("x"), __("Y()"))) //false: x!<Y
    assertTrue(he(__("x"), __("Y(x)")))//true: x<Y(x) 
    
    // from Soerensen paper:
    // b < f(b)
    assertTrue(he(__("b"), __("f(b)"))) 
    // c(b) < c(f(b))
    assertTrue(he(__("c(b)"), __("c(f(b))")))
    // C(z) < C(F(b))
    assertTrue(he(__("C(z)"), __("C(F(b))")))
    // d(b, b) < d(f(b), f(b))
    assertTrue(he(__("d(b, b)"), __("d(f(b), f(b))")))
    // f(c(b)) !< c(b)
    assertFalse(he(__("f(c(b))"), __("c(b)")))
    // f(c(b)) !< c(f(b))
    assertFalse(he(__("f(c(b))"), __("c(f(b))")))
    // f(c(b)) !< f(f(f(b)))
    assertFalse(he(__("f(c(b))"), __("f(f(f(b)))")))
  }
  
  @Test def simpleEquivalent(): Unit = {
    // B = B
    assertTrue(equivalent(__("B()"), __("B()")))
    
    // D(x, x) = D(y, y)
    assertTrue(equivalent(__("D(x, x)"), __("D(y, y)")))

    // d(x, y) = d(y, x)
    assertTrue(equivalent(__("d(x, y)"), __("d(y, x)")))
                          
    // D(x, y) != D(z, z)
    assertFalse(equivalent(__("D(x, y)"), __("D(z, z)")))
  }
  
  @Test def simpleMSG(): Unit = {
    // A(B)^B = x
    val msg0 = msg(__("A(B())"), __("B()"))
    println(msg0)
    assertTrue(equivalent(msg0.term, __("x")))
    
    // C(B)^C(F(B)) = C(x)
    val msg1 = msg(__("C(B())"), __("C(F(B()))"))
    println(msg1)
    assertTrue(equivalent(msg1.term, __("C(x)")))

    // D(B, B)^D(F(B), F(B)) = D(x, x)
    val msg2 = msg(__("D(B(), B())"), __("D(F(B()), F(B()))"))
    println(msg2)
    assertTrue(equivalent(msg2.term, __("D(x, x)")))
  }
  
  /*
  @Test def strongMSG(): Unit = {
    // A(B)^B = x
    val msg0 = strongMsg(__("A(B)"), __("B"))
    println(msg0)
    assertTrue(equivalent(msg0.term, __("x")))
    
    // C(B)^C(F(B)) = C(x)
    val msg1 = strongMsg(__("C(B)"), __("C(F(B))"))
    println(msg1)
    assertTrue(equivalent(msg1.term, __("C(x)")))

    // D(B, B)^D(F(B), F(B)) = D(x, x)
    val msg2 = strongMsg(__("D(B, B)"), __("D(F(B), F(B))"))
    println(msg2)
    assertTrue(equivalent(msg2.term, __("D(x, x)")))
    
    // C(x)^C(F(B)) = C(x)
    val msg3 = strongMsg(__("C(x)"), __("C(F(B))"))
    println(msg3)
    assertEquals(msg3.term, __("C(x)"))
    
    // C(F(B))^C(x) = C(x)
    val msg4 = strongMsg( __("C(F(B))"), __("C(x)"))
    println(msg4)
    assertEquals(msg4.term, __("C(x)"))
    
    val msg5 = strongMsg( __("a(a(xs, ys), xs)"), __("a(a(xs, ys), zs)"))
    println(msg5)
    assertEquals(msg5.term, __("a(a(xs, ys), zs)"))
    assertTrue(msg5.sub2.isEmpty)
    assertEquals(msg5.sub1.size, 1)
  }
  */
  
  @Test def simpleInstanceOf(): Unit = {
    // A(x) <~ A(B); A(x){x:=B)=A(B)
    assertTrue(instanceOf(__("A(x)"), __("A(B())")))
                          
    // A(B) !<~ A(x);
    assertFalse(instanceOf(__("A(B())"), __("A(x)")))
  }
    
  @Test def simpleIncommensurable(): Unit = {
    
    assertFalse(incommensurable(__("A(x)"), __("A(B())")))                         
    
    assertTrue(incommensurable(__("D(B(), x)"), __("A(x)")))
  }
}
