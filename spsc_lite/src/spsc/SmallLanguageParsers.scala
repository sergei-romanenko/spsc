package spsc

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.Reader

object SmallLanguageParsers extends StandardTokenParsers with ImplicitConversions {
  lexical.delimiters += ("(", ")", ",", "=", ";")
  private def program = definition+
  private def definition: Parser[Definition] = gFunction | fFunction
  private def term: Parser[Term] = call | constructor | variable
  
  private def uident = ident ^? {case id if id.charAt(0).isUpperCase => id}
  private def lident = ident ^? {case id if id.charAt(0).isLowerCase => id}
  private def variable = lident ^^ Variable
  private def pattern = uident ~ ((("(" ~> repsep(variable, ",") <~ ")")?) ^^ {_.getOrElse(Nil)} ) ^^ Pattern
  private def fFunction = lident ~ ("(" ~> repsep(variable, ",") <~ ")") ~ ("=" ~> term <~ ";") ^^ FFunction
  private def gFunction = lident ~ ("(" ~> pattern) ~ ((("," ~> variable)*) <~ ")") ~ ("=" ~> term <~ ";") ^^ GFunction
  private def call = lident ~ ("(" ~> repsep(term, ",") <~ ")") ^^ FCall
  private def constructor = uident ~ ((("(" ~> repsep(term, ",") <~ ")")?) ^^ {_.getOrElse(Nil)} ) ^^ Constructor  
  
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