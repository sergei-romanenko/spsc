package spsc;

import scala.util.parsing.input.Reader
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition

import SmallLanguage._

object SmallLanguageParsers extends StandardTokenParsers with StrongParsers {
  
  private object startsWithLowerCase extends PartialFunction[String, String] {
    def isDefinedAt(s: String) = s.charAt(0).isLowerCase
    def apply(s: String) = s
  }
  
  private object startsWithUpperCase extends PartialFunction[String, String] {
    def isDefinedAt(s: String) = s.charAt(0).isUpperCase
    def apply(s: String) = s
  }
  
  lexical.delimiters += ("(", ")", ",", "=", ";")
    
  private def variable: Parser[Variable] = 
    (ident ^? startsWithLowerCase) ^^ {Variable(_)}
  
  private def pattern: Parser[Pattern] = 
    (ident ^? (startsWithUpperCase, x => "Expected constructor, found " + x)) ~ (("(" ~> repsep(variable, ",") <~ ")")?) ^^
      {case name ~ args => args match {
        case None => Pattern(name, Nil)
        case Some(args) => Pattern(name, args)}}
  
  private def fFunction: Parser[FFunction] =
    (ident ^? (startsWithLowerCase, x => "Expected def, found " + x)) ~ ("(" ~> repsep(variable, ",") <~ ")") ~ 
       "=" ~ term ~ ";" ^^
      {case name ~ args ~ "=" ~ t ~ ";" => FFunction(name, args, t)}
  
  private def gFunction: Parser[GFunction] =
    (ident ^? (startsWithLowerCase, x => "Expected def, found " + x)) ~  
      ("(" ~> pattern  ~ ((("," ~ variable)^^{case x ~ y => y})*) <~ ")") ~ "=" ~ term ~ ";"^^
        {case name ~ (p ~ args) ~ "=" ~ t ~ ";" => GFunction(name, p, args, t)}
  
  private def call: Parser[Call] =
    (ident ^? (startsWithLowerCase, x => "Expected fun name, found " + x)) ~ ("(" ~> repsep(term, ",") <~ ")") ^^
      {case fName ~ args => FCall(fName, args)}

  private def constructor: Parser[Constructor] =
    (ident ^? (startsWithUpperCase, x => "Expected constructor, found " + x)) ~ (("(" ~> repsep(term, ",") <~ ")")?) ^^
      {case name ~ args => args match {
        case None => Constructor(name, Nil)
        case Some(args) => Constructor(name, args)}}

  private def term: Parser[Term] = call | constructor | variable
  
  private def definition: Parser[Definition] = 
    gFunction | fFunction
  
  // The main parser.
  private def program: Parser[List[Definition]] = strongRep1(definition)
  
  
  // A utility class for iterating over "definition stream".
  class DefinitionScanner(definitions: List[Definition]) extends Reader[Definition]{
    val (first, restDefinitions) = definitions match {
      case Nil => (null, Nil)
      case p :: o => (p, o)
    }
    def atEnd = first == null
    def pos = if (atEnd) NoPosition else new DefinitionPosition(first)
    def rest = new DefinitionScanner(restDefinitions)
  }
  
  class DefinitionPosition(definition: Definition) extends Position {
    def line = 0
    def column = 0
    override def toString = "Definition"
    override def longString = definition.toString
    def lineContents(lnum: Int) = ""
  }
  
  // The checker of context-dependent restrictions.
  object Validator extends StrongParsers {
    sealed abstract class TermProceedingResult
    case class TermProceedingSuccess(term: Term) extends TermProceedingResult
    case class TermProceedingFailure(errorMsg: String) extends TermProceedingResult
    
    type Elem = Definition    
    def validate(rawProgram: List[Definition]): Parser[List[Definition]] = {
      
      import scala.collection.mutable.Map
      import scala.collection.mutable.Set
      
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
      val varNames = Set[String]()
      
      // A name belongs to one and only one of the sets {g, f, v, c}.
      // The arity of constructors and functions must be consistent.
      // All constructors in the definition of a g-functions must be different.
      // A variable in a pattern can appear only once.
      // A variable appearing in the right hand side must appear in the left hand side.
      def validateDefinition: Parser[Definition] = new Parser[Definition]{
        def apply(in: Input):ParseResult[Definition] = {
          
          val definition = in.first
          val fVars = Set[Variable]()
          
          def validateTerm(t: Term): TermProceedingResult = t match {
            case v @ Variable(name) => {
              if (fInfo.contains(name))
                TermProceedingFailure(name + " is already defined as f-function ")
              else if (gInfo.contains(name))
                TermProceedingFailure(name + " is already defined as g-function ")
              else if (!fVars.contains(v)){
                TermProceedingFailure("undefined variable " + name)
              }
              else {
                varNames + name
                TermProceedingSuccess(v)
              }
            }
            case Constructor(name, args) => {
              if (cInfo.contains(name) && cInfo(name) != args.size)
                return TermProceedingFailure(name + " is already defined as constructor with arity " + cInfo(name))
              else {
                cInfo(name) = args.size
                val proceededArgs = new scala.collection.mutable.ListBuffer[Term]
                for (arg <- args) {
                  val subResult = validateTerm(arg)
                  subResult match {
                    case r: TermProceedingFailure => return r
                    case TermProceedingSuccess(t) => proceededArgs += t 
                  }
                }
                TermProceedingSuccess(Constructor(name, proceededArgs.toList))
              }
            }
            case FCall (name, args) => {
              var fCall = true
              if (fArity.contains(name)) {
                if (fArity(name) != args.size)
                  return TermProceedingFailure("Wrong numbers of arguments for function " + name) 
              } else if (gArity.contains(name)) {
                fCall = false
                if (gArity(name) != args.size)
                  return TermProceedingFailure("Wrong numbers of arguments for function " + name) 
              } else {
                return TermProceedingFailure("Call to undefined function " + name)
              }
              val proceededArgs = new scala.collection.mutable.ListBuffer[Term]
              for (arg <- args) {
                val subResult = validateTerm(arg)
                subResult match {
                  case r: TermProceedingFailure => return r
                  case TermProceedingSuccess(t) => proceededArgs += t 
                }
              }
              val proceededArgsList = proceededArgs.toList
              if (fCall) {
                TermProceedingSuccess(FCall(name, proceededArgsList))
              } else {
                TermProceedingSuccess(GCall(name, proceededArgsList.head, proceededArgsList.tail))
              }
            }
            case g: GCall => TermProceedingFailure("Internal error: g-call encountered at the second parse stage")
          }
          
          definition match {
            case FFunction(name, args, rawTerm) => {
              // 1. name of f-function must be unique.
              if (varNames.contains(name)) return Failure(name + " is already defined as variable " + name, in);
              if (fInfo.contains(name)) return Failure(name + " is already defined as f-function " + name, in);
              if (gInfo.contains(name)) return Failure(name + " is already defined as g-function " + name, in);
              fInfo += (name -> args.size)
              // 2.a No variable occurs more than once in a left side
              // 2.b Name of variable must be unique in global context
              for (v <- args){
                if (fVars.contains(v)){
                  return Failure("Variable " + v.name + " occurs more than once in a left side", in);
                }
                if (fInfo.contains(v.name)){
                  return Failure(name + " is already defined as f-function " + name, in);
                }
                if (gInfo.contains(v.name)){
                  return Failure(name + " is already defined as g-function " + name, in);
                }
                fVars + v
                varNames + v.name
              }
              validateTerm(rawTerm) match {
                case TermProceedingSuccess(term: Term) => return Success(FFunction(name, args, term), in.rest)
                case TermProceedingFailure(errorMsg) => return Failure(errorMsg, in)
              }
            }
            
            case GFunction(name, arg0, args, rawTerm) => {
              // 1. name of g-function must be unique
              if (varNames.contains(name)) return Failure(name + " is already defined as variable " + name, in);
              if (fInfo.contains(name)) return Failure(name + " is already defined as f-function " + name, in);
              
              // 2. fixed arity and constructor uniquness - also constructor global uniqueness
              val cName = arg0.name
              if (cInfo.contains(cName)){
                val cArity = cInfo(cName)
                if (cArity != arg0.args.size){
                  return Failure(cName + " is already defined as constructor with arity " + cArity, in);
                }
              }
              if (gInfo.contains(name)) {
                val (arity, cNames) = gInfo(name)
                if (arity != args.size + 1){
                  return Failure(name + " is already defined as g-function with arity " + arity, in);
                }
                if (cNames.contains(cName)) {
                  return Failure("g-function " + name + " with constructor " + cName + " is already defined", in);
                }
                cNames + cName
              } else {
                gInfo(name) = (args.size + 1, Set(cName))
              }
              // 3.a A variable can appear in a left side no more than once.
              // 3.b A variable name must be unique in the global context.
              for (v <- (arg0.args ::: args)){
                if (fVars.contains(v)){
                  return Failure("Variable " + v.name + " occurs more than once in a left side", in);
                }
                if (fInfo.contains(v.name)){
                  return Failure(name + " is already defined as f-function " + name, in);
                }
                if (gInfo.contains(v.name)){
                  return Failure(name + " is already defined as g-function " + name, in);
                }
                fVars + v
                varNames + v.name
              }
              validateTerm(rawTerm) match {
                case TermProceedingSuccess(term: Term) => return Success(GFunction(name, arg0, args, term), in.rest)
                case TermProceedingFailure(errorMsg) => return Failure(errorMsg, in)
              }
            }
            
          }          
        }
      }
      
      strongRep1(validateDefinition)
    }
  }
  
  def parseProgram(r: Reader[Char]): ParseResult[List[Definition]] = {
    val result0 = program(new lexical.Scanner(r))
    if (result0.successful) 
      Validator.validate(result0.get)(new DefinitionScanner(result0.get)).asInstanceOf[ParseResult[List[Definition]]]
    else
      result0
  }
  
  // Since the term is parsed without a context,
  // all calls are temporary classified as f-calls.
  def parseTerm(r: Reader[Char]): ParseResult[Term] = strong(term)(new lexical.Scanner(r))
  
}