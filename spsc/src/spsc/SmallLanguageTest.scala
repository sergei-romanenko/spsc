package spsc;

import scala.util.parsing.input.CharArrayReader
import org.junit.Test
import org.junit.Assert._
import SmallLanguage._

class SmallLanguageParsersTest {
  @Test def simple01(): Unit ={
    val program = 
    """
    |a(x) = x;
    """
    
    val expected = 
      Definition(FPattern("a", List(Variable("x"))), Variable("x")) :: Nil
    val in = new CharArrayReader(program.stripMargin.toCharArray)
    val result = SmallLanguageParsers.parseProgram(in)
    println(result)
    assertTrue(result.successful)
    assertEquals(expected, result.get)    
  }
  
  @Test def simple02(): Unit ={
    val program = 
    """
    |a(C) = C;
    """
    
    val expected = 
      Definition(GPattern("a", List(Constructor("C", Nil))), Constructor("C", Nil)) :: Nil
    val in = new CharArrayReader(program.stripMargin.toCharArray)
    val result = SmallLanguageParsers.parseProgram(in)
    println(result)
    assertTrue(result.successful)
    assertEquals(expected, result.get)
  }
  
  @Test def simple03(): Unit ={
    val program = 
    """
    |a(Nil, vs) = vs;
    |a(Cons(u, us), vs) = Cons(u, a(us, vs));
    """
        
    val expected = 
      Definition(GPattern("a", List(Constructor("Nil", Nil), Variable("vs"))), Variable("vs")) ::
      Definition(GPattern("a", List(Constructor("Cons", List(Variable("u"), Variable("us"))), Variable("vs"))), 
          Constructor("Cons", List(Variable("u"), Call("a", List(Variable("us"), Variable("vs")), CallType.G)))) :: Nil
    val in = new CharArrayReader(program.stripMargin.toCharArray)
    val result = SmallLanguageParsers.parseProgram(in)
    println(result)
    assertTrue(result.successful)
    assertEquals(expected, result.get)    
  }
  
  @Test def parserError01(): Unit ={
    val program = 
    """
    |a(Nil, Nil) = Nil;
    """
    
    val in = new CharArrayReader(program.stripMargin.toCharArray)
    val result = SmallLanguageParsers.parseProgram(in)
    println(result)
    assertFalse("Input is incorrect. Parser should fail.", result.successful)    
  }
  
  @Test def syntaxError01(): Unit ={
    val program = 
    """
    |a(x, x) = x;
    """
    
    val in = new CharArrayReader(program.stripMargin.toCharArray)
    val result = SmallLanguageParsers.parseProgram(in)
    println(result)
    assertFalse("Input is incorrect. Validator should find error.", result.successful)    
  }
  
}
