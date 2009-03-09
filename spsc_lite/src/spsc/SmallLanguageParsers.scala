package spsc

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.{Positional, Reader, Position}
import scala.util.parsing.syntax.StdTokens
import scala.collection.mutable.{ListBuffer, Map, Set}

object SmallLanguageParsers extends STokenParsers with StrongParsers with ImplicitConversions {
  def p[T <: Positional](p: => Parser[T]): Parser[T] = positioned(p)
  
  lexical.delimiters += ("(", ")", ",", "=", ";")
  
  private def variable: Parser[Variable] = 
    p(lident ^^ Variable)
  
  private def pattern: Parser[Pattern] = 
    p(uident ~ ((("(" ~> repsep(variable, ",") <~ ")")?) ^^ {case None => Nil; case Some(s) => s}) ^^ Pattern)
  
  private def fFunction: Parser[FFunction] =
    p(lident ~ ("(" ~> repsep(variable, ",") <~ ")") ~ ("=" ~> term <~ ";") ^^ FFunction)
  
  private def gFunction: Parser[GFunction] =
    p(lident ~ ("(" ~> pattern)  ~ (( ("," ~> variable)*) <~ ")") ~ ("=" ~> term <~ ";") ^^ GFunction)
  
  private def call: Parser[Call] =
    p(lident ~ ("(" ~> repsep(term, ",") <~ ")") ^^ FCall)

  private def constructor: Parser[Constructor] =
    p(uident ~ ((("(" ~> repsep(term, ",") <~ ")")?) ^^ {case None => Nil; case Some(s) => s} ) ^^ Constructor)

  private def term: Parser[Term] = call | constructor | variable
  
  private def definition: Parser[Definition] = gFunction | fFunction
  
  // The main parser.
  private def program: Parser[List[Definition]] = strongRep1(definition)  
     
  def validate(rawResult: ParseResult[List[Definition]]): ParseResult[List[Definition]] = {      
    val rawProgram = rawResult.get      
    val fArity = Map[String, Int]()
    val gArity = Map[String, Int]()
      
    for (definition <- rawProgram) definition match {
      case FFunction(name, args, _) => 
        if (!fArity.contains(name) && !gArity.contains(name)) fArity + (name -> args.size)
      case GFunction(name, _, args, _) => 
        if (!fArity.contains(name) && !gArity.contains(name)) gArity + (name -> (args.size + 1))
    }
      
    val cInfo = Map[String, Int]() // arity for constructor
    val fInfo = Map[String, Int]() // arity for f-function
    val gInfo = Map[String, Pair[Int, Set[String]]]() // name -> (arity, constructor names)
      
    // A name belongs to one and only one of the sets {g, f, v, c}.
    // The arity of constructors and functions must be consistent.
    // All constructors in the definition of a g-functions must be different.
    // A variable in a pattern can appear only once.
    // A variable appearing in the right hand side must appear in the left hand side.
    def validate(definition: Definition): ParseResult[Definition] =  {
      val fVars = Set[Variable]()
        
      def validateTerm(t: Term): ParseResult[Term] = t match {
        case v @ Variable(name) => {
          if (!fVars.contains(v)){
            error("undefined variable " + name, v)
          }
          else {
            Success(v, null)
          }
        }
        case c@Constructor(name, args) => {
          if (cInfo.contains(name) && cInfo(name) != args.size)
            return error(name + " is already defined as constructor with arity " + cInfo(name), c)
          else {
            cInfo(name) = args.size
            val proceededArgs = new ListBuffer[Term]
            for (arg <- args) {
              val subResult = validateTerm(arg)
              subResult match {
                case r: NoSuccess => return r
                case Success(t, _) => proceededArgs += t 
              }
            }
            Success(Constructor(name, proceededArgs.toList), null)
          }
        }
        case fc@FCall (name, args) => {
          var fCall = true
          if (fArity.contains(name)) {
            if (fArity(name) != args.size)
              return error("Wrong numbers of arguments for function " + name, fc) 
          } else if (gArity.contains(name)) {
            fCall = false
            if (gArity(name) != args.size)
              return error("Wrong numbers of arguments for function " + name, fc) 
          } else {
            // we assume that there is call to undefined f-function
            fArity(name) = args.size
          }
          val proceededArgs = new ListBuffer[Term]
          for (arg <- args) {
            val subResult = validateTerm(arg)
            subResult match {
              case r: NoSuccess => return r
              case Success(t, _) => proceededArgs += t 
            }
          }
          val proceededArgsList = proceededArgs.toList
          if (fCall) {
            Success(FCall(name, proceededArgsList), null)
          } else {
            Success(GCall(name, proceededArgsList.head, proceededArgsList.tail), null)
          }
        }
        case g: GCall => error("Internal error: g-call encountered at the second parse stage", g)
      }
      
      definition match {
        case FFunction(name, args, rawTerm) => {
          // 1. name of f-function must be unique.
          if (fInfo.contains(name)) return error(name + " is already defined as f-function " + name, definition);
          if (gInfo.contains(name)) return error(name + " is already defined as g-function " + name, definition);
          fInfo += (name -> args.size)
          // 2.a No variable occurs more than once in a left side
          // 2.b Name of variable must be unique in global context
          for (v <- args){
            if (fVars.contains(v)){
              return error("Variable " + v.name + " occurs the second time in a left side", v);
            }
            fVars + v
          }
          validateTerm(rawTerm) match {
            case Success(term: Term, _) => return Success(FFunction(name, args, term), null)
            case r: NoSuccess => return r
          }
        }
        
        case GFunction(name, arg0, args, rawTerm) => {
          // 1. name of g-function must be unique
          if (fInfo.contains(name)) return error(name + " is already defined as f-function " + name, definition);
          // 2. fixed arity and constructor uniquness - also constructor global uniqueness
          val cName = arg0.name
          if (cInfo.contains(cName)){
            val cArity = cInfo(cName)
            if (cArity != arg0.args.size){
              return error(cName + " is already defined as constructor with arity " + cArity, arg0);
            }
          } else {
            cInfo(cName) = arg0.args.size
          }
          if (gInfo.contains(name)) {
            val (arity, cNames) = gInfo(name)
            if (arity != args.size + 1){
              return error(name + " is already defined as g-function with arity " + arity, definition);
            }
            if (cNames.contains(cName)) {
              return error("g-function " + name + " with constructor " + cName + " is already defined", definition);
            }
            cNames + cName
          } else {
            gInfo(name) = (args.size + 1, Set(cName))
          }
          // 3.a A variable can appear in a left side no more than once.
          for (v <- (arg0.args ::: args)){
            if (fVars.contains(v)){
              return error("Variable " + v.name + " occurs the second time in a left side", v);
            }
            fVars + v
          }
          validateTerm(rawTerm) match {
            case Success(term, _) => return Success(GFunction(name, arg0, args, term), null)
            case r: NoSuccess => return r
          }
        }                
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
  
  def parseProgram(r: Reader[Char]): ParseResult[List[Definition]] = {
    val result0 = program(new lexical.Scanner(r))
    if (result0.successful) 
      validate(result0)
    else
      result0
  }
  
  // Since the term is parsed without a context,
  // all calls are temporary classified as f-calls.
  def parseTerm(r: Reader[Char]): ParseResult[Term] = strong(term)(new lexical.Scanner(r))
  
  def error(msg: String, pos: Positional) = {
    lastNoSuccess = null; val e = SError(msg, pos);  lastNoSuccess = null; e
  }
  
  case class SError(override val msg: String, val pos: Positional) extends Error(msg, null) {
    override def toString = "[" + pos.pos +"] error: "+msg+"\n\n"+pos.pos.longString
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