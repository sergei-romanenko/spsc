package spsc

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.Reader

object SmallLanguageParsers extends StandardTokenParsers with ImplicitConversions {
  lexical.delimiters += ("(", ")", ",", "=", ";")
  def program = definition+
  def definition: Parser[Definition] = gFunction | fFunction
  def term: Parser[Term] = call | constructor | variable
  
  def uid = ident ^? {case id if id.charAt(0).isUpperCase => id}
  def lid = ident ^? {case id if id.charAt(0).isLowerCase => id}
  def variable = lid ^^ Variable
  def pattern = uid ~ ("(" ~> repsep(variable, ",") <~ ")") ^^ Pattern
  def fFunction = lid ~ ("(" ~> repsep(variable, ",") <~ ")") ~ ("=" ~> term <~ ";") ^^ FFunction
  def gFunction = lid ~ ("(" ~> pattern) ~ ((("," ~> variable)*) <~ ")") ~ ("=" ~> term <~ ";") ^^ GFunction
  def call = lid ~ ("(" ~> repsep(term, ",") <~ ")") ^^ FCall
  def constructor = uid ~ ("(" ~> repsep(term, ",") <~ ")") ^^ Constructor  
  
  def parseProgram(r: Reader[Char]): List[Definition] = postProcess(program(new lexical.Scanner(r)).get)
  def parseTerm(r: Reader[Char]): ParseResult[Term] = term(new lexical.Scanner(r))
  
  def postProcess(rawProgram: List[Definition]): List[Definition] = {   
    var gs: Set[String] = Set.empty  
    for (definition <- rawProgram) definition match {
      case GFunction(name, _, _, _) => gs = gs + name
      case _ => 
    }
    def walkTerm(t: Term): Term = t match {
      case v@Variable(_) => v
      case c@Constructor(name, args) => Constructor(name, args map walkTerm)
      case fc@FCall (name, args) =>
        if (gs.contains(name))
          GCall(name, walkTerm(args.head), args.tail map walkTerm)
        else
          FCall(name, args map walkTerm)
	}
    def walkFun(definition: Definition): Definition = definition match {
      case FFunction(name, args, rawTerm) => FFunction(name, args, walkTerm(rawTerm))
      case GFunction(name, arg0, args, rawTerm) => GFunction(name, arg0, args, walkTerm(rawTerm))
    }      
    rawProgram map walkFun
  }
}