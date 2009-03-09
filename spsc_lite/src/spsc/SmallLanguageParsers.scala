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
  private def pattern = uident ~ ((("(" ~> repsep(variable, ",") <~ ")")?) ^^ {_.getOrElse(Nil)}) ^^ Pattern
  private def fFunction = lident ~ ("(" ~> repsep(variable, ",") <~ ")") ~ ("=" ~> term <~ ";") ^^ FFunction
  private def gFunction = lident ~ ("(" ~> pattern) ~ ((("," ~> variable)*) <~ ")") ~ ("=" ~> term <~ ";") ^^ GFunction
  private def call = lident ~ ("(" ~> repsep(term, ",") <~ ")") ^^ FCall
  private def constructor = uident ~ ((("(" ~> repsep(term, ",") <~ ")")?) ^^ {_.getOrElse(Nil)} ) ^^ Constructor  
  
  def parseProgram(r: Reader[Char]): ParseResult[List[Definition]] = {
    val result0 = program(new lexical.Scanner(r))
    if (result0.successful) 
      validate(result0)
    else
      result0
  }

  def parseTerm(r: Reader[Char]): ParseResult[Term] = term(new lexical.Scanner(r))
  
  def validate(rawResult: ParseResult[List[Definition]]): ParseResult[List[Definition]] = {      
    val rawProgram = rawResult.get      
    val gs: Set[String] = Set.empty
      
    for (definition <- rawProgram) definition match {
      case GFunction(name, _, _, _) => gs += name
      case _ => 
    }

    def validate(definition: Definition): ParseResult[Definition] =  {
      def validateTerm(t: Term): Term = t match {
        case v @ Variable(_) => v
        case c@Constructor(name, args) => Constructor(name, args map validateTerm)
        case fc@FCall (name, args) =>
          if (gs.contains(name)) {
            GCall(name, validateTerm(args.head), args.tail map validateTerm)
          } else {
            FCall(name, args map validateTerm)
          }
        }
      definition match {
        case FFunction(name, args, rawTerm) => return Success(FFunction(name, args, validateTerm(rawTerm)), null)
        case GFunction(name, arg0, args, rawTerm) => return Success(GFunction(name, arg0, args, validateTerm(rawTerm)), null)
      }      
    }
    val ds = new ListBuffer[Definition]()
    for (d <- rawProgram) {
      validate(d) match {
        case r: NoSuccess => return r
        case Success(vd, _) => ds + vd
      }
    }
    Success(ds.toList, rawResult.next)   
  }
  
}

class STokenParsers extends StdTokenParsers {  
  type Tokens = StdTokens
  val lexical = new StdLexical
  
  import lexical.Identifier
  
  def uident: Parser[String] = 
    elem("uidentifier", x => x.isInstanceOf[Identifier] && x.chars.charAt(0).isUpperCase) ^^ (_.chars)
    
  def lident: Parser[String] = 
    elem("lidentifier", x => x.isInstanceOf[Identifier] && x.chars.charAt(0).isLowerCase) ^^ (_.chars)  
}