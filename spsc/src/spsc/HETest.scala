package spsc;

import org.junit.Test
import org.junit.Assert._
import FreeAlgebra._
import HE.he

// TODO : via parser
class HETest {  
  
  @Test def simpleHE() : Unit = {
    assertTrue(he(AVar("x"), AVar("y"))) //true: x<y
    assertFalse(he(AVar("x"), ASym("Y", Nil))) //false: x!<Y
    assertTrue(he(AVar("x"), ASym("Y", AVar("x")::Nil)))//true: x<Y(x) 
    assertTrue(he(ASym("Y", List()), ASym("Y", Nil)))
    
    // from Sorensen paper:
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
}
