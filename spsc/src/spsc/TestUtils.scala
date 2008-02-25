package spsc;

import scala.util.parsing.input.CharArrayReader

import org.junit.Test
import org.junit.Assert._
import SmallLanguage._

object TestUtils {
  def termFromString(input: String) = SmallLanguageParsers.parseTerm(new CharArrayReader(input.toCharArray)).get
  
  def programFromString(input: String) = parseResultFromString(input).get
  
  def parseResultFromString(input: String) = SmallLanguageParsers.parseProgram(new CharArrayReader(input.toCharArray))
}
