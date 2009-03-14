package spsc

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.Reader

object SLanguageParsers extends StandardTokenParsers with ImplicitConversions {
  lexical.delimiters += ("(", ")", ",", "=", ";")
  def program = definition+
  def definition: Parser[Definition] = gFunction | fFunction
  def term: Parser[Term] = fcall | gcall | constructor | variable
  
  def uid = ident ^? {case id if id.charAt(0).isUpperCase => id}
  def lid = ident ^? {case id if id.charAt(0).isLowerCase => id}
  def fid = ident ^? {case id if id.charAt(0) == 'f' => id}
  def gid = ident ^? {case id if id.charAt(0) == 'g' => id}
  def variable = lid ^^ Variable
  def pattern = uid ~ ("(" ~> repsep(variable, ",") <~ ")") ^^ Pattern
  def fFunction = fid ~ ("(" ~> repsep(variable, ",") <~ ")") ~ ("=" ~> term <~ ";") ^^ FFunction
  def gFunction = gid ~ ("(" ~> pattern) ~ ((("," ~> variable)*) <~ ")") ~ ("=" ~> term <~ ";") ^^ GFunction
  def constructor = uid ~ ("(" ~> repsep(term, ",") <~ ")") ^^ Constructor 
  def fcall = fid ~ ("(" ~> repsep(term, ",") <~ ")") ^^ FCall
  def gcall = gid ~ ("(" ~> repsep(term, ",") <~ ")") ^^ GCall
  
  def parseProgram(r: Reader[Char]) = Program(program(new lexical.Scanner(r)).get)
  def parseTerm(r: Reader[Char]) = term(new lexical.Scanner(r)).get
  
  def parseProgram2(r: Reader[Char]) = program(new lexical.Scanner(r)).get
}