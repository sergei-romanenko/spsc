package spsc;

import org.junit.Test
import org.junit.Assert._

class SmallLanguageParsersTest {
  @Test def simple01(): Unit = {
    val programText = 
    """
    |a(x) = x;
    """
    
    val expected = 
      FFunction("a", List(Variable("x")), Variable("x")) :: Nil
    val result = TestUtils.parseResultFromString(programText.stripMargin)    
    println(result)
    assertEquals(expected, result)    
  }
  
  @Test def simple02(): Unit ={
    val programText = 
    """
    |a(C()) = C();
    """
    
    val expected = 
      GFunction("a",
          Pattern("C", Nil),
          Nil,
          Constructor("C", Nil)) :: Nil
    val result = TestUtils.parseResultFromString(programText.stripMargin)
    println(result)
    assertEquals(expected, result)
  }
  
  @Test def simple03(): Unit ={
    val programText = 
    """
    |a(Nil(), vs) = vs;
    |a(Cons(u, us), vs) = Cons(u, a(us, vs));
    """
        
    val expected = 
      GFunction("a",
          Pattern("Nil", Nil),
          List(Variable("vs")),
          Variable("vs")) ::
      GFunction("a",
          Pattern("Cons", List(Variable("u"), Variable("us"))),
          List(Variable("vs")), 
          Constructor("Cons", List(Variable("u"), GCall("a", Variable("us"), List( Variable("vs")))))) :: 
      Nil
    val result = TestUtils.parseResultFromString(programText.stripMargin)
    println(result)
    assertEquals(expected, result)    
  }
  
  @Test def simple04(): Unit ={
    val programText = 
    """
    |a(Nil(), vs) = b(vs);
    |a(Cons(u, us), vs) = Cons(u, a(us, vs));
    """
        
    val expected = 
      GFunction("a",
          Pattern("Nil", Nil),
          List(Variable("vs")),
          FCall("b", Variable("vs")::Nil)) ::
      GFunction("a",
          Pattern("Cons", List(Variable("u"), Variable("us"))),
          List(Variable("vs")), 
          Constructor("Cons", List(Variable("u"), GCall("a", Variable("us"), List( Variable("vs")))))) :: 
      Nil
    val result = TestUtils.parseResultFromString(programText.stripMargin)
    println(result)
    assertEquals(expected, result)    
  }
  
  @Test def parserError01(): Unit ={
    val programText = 
    """
    |a(Nil, Nil) = Nil;
    """
    try{
      val result = TestUtils.parseResultFromString(programText.stripMargin)
      println(result)
      fail
    } catch {
      case e => 
    }
    
    //assertFalse("Input is incorrect. Parser should fail.", result.successful)    
  }

}
