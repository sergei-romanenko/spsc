package spsc;

import org.junit.Test
import org.junit.Assert._
import SmallLanguage._

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
    assertTrue(result.successful)
    assertEquals(expected, result.get)    
  }
  
  @Test def simple02(): Unit ={
    val programText = 
    """
    |a(C) = C;
    """
    
    val expected = 
      GFunction("a",
          Pattern("C", Nil),
          Nil,
          Constructor("C", Nil)) :: Nil
    val result = TestUtils.parseResultFromString(programText.stripMargin)
    println(result)
    assertTrue(result.successful)
    assertEquals(expected, result.get)
  }
  
  @Test def simple03(): Unit ={
    val programText = 
    """
    |a(Nil, vs) = vs;
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
    assertTrue(result.successful)
    assertEquals(expected, result.get)    
  }
  
  @Test def simple04(): Unit ={
    val programText = 
    """
    |a(Nil, vs) = b(vs);
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
    assertTrue(result.successful)
    assertEquals(expected, result.get)    
  }
  
  @Test def parserError01(): Unit ={
    val programText = 
    """
    |a(Nil, Nil) = Nil;
    """
    
    val result = TestUtils.parseResultFromString(programText.stripMargin)
    println(result)
    assertFalse("Input is incorrect. Parser should fail.", result.successful)    
  }
  
  @Test def syntaxError01(): Unit ={
    val programText = 
    """
    |a(x, x) = x;
    """
    
    val result = TestUtils.parseResultFromString(programText.stripMargin)
    println(result)
    assertFalse("Input is incorrect. Validator should find error.", result.successful)    
  }
  
  @Test def syntaxError02(): Unit ={
    val programText = 
    """
    |a(x, y) = a(y);
    """
    
    val result = TestUtils.parseResultFromString(programText.stripMargin)
    println(result)
    assertFalse("Input is incorrect. Validator should find error.", result.successful)    
  }
  
  @Test def syntaxError03(): Unit ={
    val programText = 
    """
    |a(x, y) = c(y);
    |b(x, y) = c(y, x);
    """
    
    val result = TestUtils.parseResultFromString(programText.stripMargin)
    println(result)
    assertFalse("Input is incorrect. Validator should find error.", result.successful)    
  }
  
}
