package spsc;

import org.junit.Test
import org.junit.Assert._

class SmallLanguageParsersTest {
  @Test def simple01(): Unit = {
    val programText = 
    """
    |fA(x) = x;
    """
    
    val expected = 
      FFunction("fA", List(Variable("x")), Variable("x")) :: Nil
    val result = TestUtils.defs(programText.stripMargin)    
    println(result)
    assertEquals(expected, result)    
  }
  
  @Test def simple02(): Unit ={
    val programText = 
    """
    |gA(C()) = C();
    """
    
    val expected = 
      GFunction("gA",
          Pattern("C", Nil),
          Nil,
          Constructor("C", Nil)) :: Nil
    val result = TestUtils.defs(programText.stripMargin)
    println(result)
    assertEquals(expected, result)
  }
  
  @Test def simple03(): Unit ={
    val programText = 
    """
    |gA(Nil(), vs) = vs;
    |gA(Cons(u, us), vs) = Cons(u, gA(us, vs));
    """
        
    val expected = 
      GFunction("gA",
          Pattern("Nil", Nil),
          List(Variable("vs")),
          Variable("vs")) ::
      GFunction("gA",
          Pattern("Cons", List(Variable("u"), Variable("us"))),
          List(Variable("vs")), 
          Constructor("Cons", List(Variable("u"), GCall("gA", Variable("us") :: List( Variable("vs")))))) :: 
      Nil
    val result = TestUtils.defs(programText.stripMargin)
    println(result)
    assertEquals(expected, result)    
  }
  
  @Test def simple04(): Unit ={
    val programText = 
    """
    |gA(Nil(), vs) = fB(vs);
    |gA(Cons(u, us), vs) = Cons(u, gA(us, vs));
    """
        
    val expected = 
      GFunction("gA",
          Pattern("Nil", Nil),
          List(Variable("vs")),
          FCall("fB", Variable("vs")::Nil)) ::
      GFunction("gA",
          Pattern("Cons", List(Variable("u"), Variable("us"))),
          List(Variable("vs")), 
          Constructor("Cons", List(Variable("u"), GCall("gA", Variable("us") :: List( Variable("vs")))))) :: 
      Nil
    val result = TestUtils.defs(programText.stripMargin)
    println(result)
    assertEquals(expected, result)    
  }
  
  @Test def parserError01(): Unit ={
    val programText = 
    """
    |a(Nil, Nil) = Nil;
    """
    try{
      val result = TestUtils.defs(programText.stripMargin)
      println(result)
      fail
    } catch {
      case e => 
    }
    
    //assertFalse("Input is incorrect. Parser should fail.", result.successful)    
  }

}
