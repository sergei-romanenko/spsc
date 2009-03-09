package spsc

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.{Positional, Reader, Position}
import scala.util.parsing.syntax.StdTokens
import scala.collection.mutable.{ListBuffer, Map, Set}

object SmallLanguageParsers extends STokenParsers with ImplicitConversions {
  lexical.delimiters += ("(", ")", ",", "=", ";")

  private def program = definition+
  private def definition: Parser[Definition] = gFunction | fFunction
  private def term: Parser[Term] = call | constructor | variable
  
  private def variable = lident ^^ Variable
  private def pattern = uident ~ ((("(" ~> repsep(variable, ",") <~ ")")?) ^^ {_.getOrElse(Nil)} ) ^^ Pattern
  private def fFunction = lident ~ ("(" ~> repsep(variable, ",") <~ ")") ~ ("=" ~> term <~ ";") ^^ FFunction
  private def gFunction = lident ~ ("(" ~> pattern) ~ ((("," ~> variable)*) <~ ")") ~ ("=" ~> term <~ ";") ^^ GFunction
  private def call = lident ~ ("(" ~> repsep(term, ",") <~ ")") ^^ FCall
  private def constructor = uident ~ ((("(" ~> repsep(term, ",") <~ ")")?) ^^ {_.getOrElse(Nil)} ) ^^ Constructor  
  
  def parseProgram(r: Reader[Char]): List[Definition] = postProcess(program(new lexical.Scanner(r)).get)
  def parseTerm(r: Reader[Char]): ParseResult[Term] = term(new lexical.Scanner(r))
  
  def postProcess(rawProgram: List[Definition]): List[Definition] = {   
    val gs: Set[String] = Set.empty  
    for (definition <- rawProgram) definition match {
      case GFunction(name, _, _, _) => gs += name
      case _ => 
    }
    def walkTerm(t: Term): Term = t match {
      case v @ Variable(_) => v
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

class STokenParsers extends StdTokenParsers {  
  type Tokens = StdTokens
  val lexical = new StdLexical
  import lexical.Identifier
  def uident: Parser[String] = elem("uidentifier", x => x.isInstanceOf[Identifier] && x.chars.charAt(0).isUpperCase) ^^ (_.chars)
  def lident: Parser[String] = elem("lidentifier", x => x.isInstanceOf[Identifier] && x.chars.charAt(0).isLowerCase) ^^ (_.chars)  
}