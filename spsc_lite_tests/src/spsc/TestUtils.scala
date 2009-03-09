package spsc;

import scala.util.parsing.input.CharArrayReader

import org.junit.Test
import org.junit.Assert._

object TestUtils {
  def termFromString(input: String) =
    SmallLanguageParsers.parseTerm(new CharArrayReader(input.toCharArray)).get
  
  def programFromString(input: String) =
    new Program( parseResultFromString(input) )
  
  def parseResultFromString(input: String) =
    SmallLanguageParsers.parseProgram(new CharArrayReader(input.toCharArray))
    
  def runTest(
      programAsString: String,
      expr: String,
      expectedAsString: String): Unit =
  {
    val program = TestUtils.programFromString(programAsString.stripMargin)    
    val interpreter = new Interpreter(program)
    val expected = TestUtils.termFromString(expectedAsString) 
    val actual = interpreter.eval(expr)

    println(actual)
    println(expected)
    assertEquals(expected, actual)
  }
}
