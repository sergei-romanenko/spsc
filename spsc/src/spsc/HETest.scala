package spsc;

import org.junit.Test
import org.junit.Assert._

// TODO : via parser
class HETest {  
  import HE._
  
  @Test def simpleHE() : Unit = {
    assertTrue(he(HEVar("x"), HEVar("y"))) //true: x<y
    assertFalse(he(HEVar("x"), HESym("Y", Nil))) //false: x!<Y
    assertTrue(he(HEVar("x"), HESym("Y", HEVar("x")::Nil)))//true: x<Y(x) 
    assertTrue(he(HESym("Y", List()), HESym("Y", Nil)))
    
    // from Sorensen paper:
    // b < f(b)
    assertTrue(he(HESym("b", Nil), HESym("f", HESym("b", Nil) :: Nil)))
    // c(b) < c(f(b))
    assertTrue(he(HESym("c", HESym("b", Nil) :: Nil), 
                  HESym("c", HESym("f", HESym("b", Nil)::Nil) :: Nil)))                  
    // d(b, b) < d(f(b), f(b))
    assertTrue(he(HESym("d", HESym("b", Nil) :: HESym("b", Nil) :: Nil), 
                  HESym("d", HESym("f", HESym("b", Nil) :: Nil) :: HESym("f", HESym("b", Nil) :: Nil) :: Nil)))
    // f(c(b)) !< c(b)
    assertFalse(he(HESym("f", HESym("c", HESym("b", Nil)::Nil) :: Nil), 
                   HESym("c", HESym("b", Nil) :: Nil)))
    // f(c(b)) !< c(f(b))
    assertFalse(he(HESym("f", HESym("c", HESym("b", Nil)::Nil) :: Nil), 
                   HESym("c", HESym("f", HESym("b", Nil)::Nil) :: Nil)))
    // f(c(b)) !< f(f(f(b)))
    assertFalse(he(HESym("f", HESym("c", HESym("b", Nil)::Nil) :: Nil), 
                   HESym("f", HESym("f", HESym("f", HESym("b", Nil)::Nil)::Nil) :: Nil)))
  }
}
