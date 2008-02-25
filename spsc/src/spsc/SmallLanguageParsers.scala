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
  
  private def constructorDefinition: Parser[Constructor] = 
    (ident ^? (startsWithUpperCase, x => "Expected constructor, found " + x)) ~ (("(" ~> repsep(variable, ",") <~ ")")?) ^^
      {case name ~ args => args match {
        case None => Constructor(name, Nil)
        case Some(args) => Constructor(name, args)}}
  
  private def fPattern: Parser[FPattern] =
    (ident ^? (startsWithLowerCase, x => "Expected def, found " + x)) ~ ("(" ~> repsep(variable, ",") <~ ")") ^^
      {case name ~ args => FPattern(name, args)}
  
  private def gPattern: Parser[GPattern] =
    (ident ^? (startsWithLowerCase, x => "Expected def, found " + x)) ~  
      ("(" ~> constructorDefinition  ~ ((("," ~ variable)^^{case x ~ y => y})*) <~ ")") ^^
        {case name ~ (p ~ args) => GPattern(name, p :: args)}
  
  private def call: Parser[Call] =
    (ident ^? (startsWithLowerCase, x => "Expected fun name, found " + x)) ~ ("(" ~> repsep(term, ",") <~ ")") ^^
      {case fName ~ args => Call(fName, args, CallType.Unknown)}

  private def constructor: Parser[Constructor] =
    (ident ^? (startsWithUpperCase, x => "Expected constructor, found " + x)) ~ (("(" ~> repsep(term, ",") <~ ")")?) ^^
      {case name ~ args => args match {
        case None => Constructor(name, Nil)
        case Some(args) => Constructor(name, args)}}

  private def term: Parser[Term] = call | constructor | variable
  
  private def definition: Parser[Definition] =
    (gPattern | fPattern) ~ "=" ~ term ~ ";" ^^ {case pattern ~ "=" ~ term ~ ";" => Definition(pattern, term)}
  
  // main parser
  private def program: Parser[List[Definition]] = strongRep1(definition)
  
  
  // utility class for iterating over "definition stream"
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
  
  // check semantic and correctness of raw program
  object Validator extends StrongParsers {
    type Elem = Definition    
    def validate(rawProgram: List[Definition]): Parser[List[Definition]] = {
      
      import scala.collection.mutable.Map
      import scala.collection.mutable.Set
      
      val fArity = Map[String, Int]()
      val gArity = Map[String, Int]()
      
      for (definition <- rawProgram) definition.pattern match {
        case FPattern(name, args) => 
          if (!fArity.contains(name) && !gArity.contains(name)) fArity + (name -> args.size)
        case GPattern(name, args) => 
          if (!fArity.contains(name) && !gArity.contains(name)) gArity + (name -> args.size)
      }
      
      val cInfo = Map[String, Int]() // arity for constructor
      val fInfo = Map[String, Int]() // arity for f-function
      val gInfo = Map[String, Pair[Int, Set[String]]]() // name -> (arity, constructor names)
      val varNames = Set[String]()
      
      // name belongs to one set of {g, f, v, c}
      // arity is constant
      // different constructors for g-functions
      // single occurence of variable in pattern
      // on variables on the right side of definition must be present in ints left side
      def validateDefinition: Parser[Definition] = new Parser[Definition]{
        def apply(in: Input):ParseResult[Definition] = {
          
          val p = in.first
          val fVars = Set[Variable]()
          
              def validateTerm(t: Term): Option[String] = t match {
                case v @ Variable(name) => {
                  if (fInfo.contains(name))
                    Some(name + " is already defined as f-function ")
                  else if (gInfo.contains(name))
                    Some(name + " is already defined as g-function ")
                  else if (!fVars.contains(v)){
                    Some("undefined variable " + name)
                  }
                  else {
                    varNames + name
                    None
                  }
                }
                case Constructor(name, args) => {
                  if (cInfo.contains(name) && cInfo(name) != args.size)
                    return Some(name + " is already defined as constructor with arity " + cInfo(name))
                  else {
                    cInfo(name) = args.size
                    for (arg <- args) {
                      val subError = validateTerm(arg)
                      if (!subError.isEmpty){
                        return subError
                      }
                    }
                    None
                  }
                }
                case c @ Call (name, args, _) => {
                  if (fArity.contains(name)) {
                    c.callType = CallType.F
                    if (fArity(name) != args.size)
                      return Some("Wrong numbers of arguments for function " + name) 
                  } else if (gArity.contains(name)) {
                    c.callType = CallType.G
                    if (gArity(name) != args.size)
                      return Some("Wrong numbers of arguments for function " + name) 
                  } else {
                    return Some("Call to undefined function " + name)
                  }
                  for (arg <- args) {
                    val subError = validateTerm(arg)
                    if (!subError.isEmpty){
                      return subError
                    }
                  }
                  None
                }
              }
          
          p.pattern match {
            case FPattern(name, args) => {
              // 1. name of f-function must be unique.
              if (varNames.contains(name)) return Failure(name + " is already defined as variable " + name, in);
              if (fInfo.contains(name)) return Failure(name + " is already defined as f-function " + name, in);
              if (gInfo.contains(name)) return Failure(name + " is already defined as g-function " + name, in);
              fInfo += (name -> args.size)
              // 2.a No variable occurs more than once in a left side
              // 2.b Name of variable must be unique in global context
              for (arg <- args){
                val v = arg.asInstanceOf[Variable]
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
            }
            case GPattern(name, args) => {
              // 1. name of g-function must be unique
              if (varNames.contains(name)) return Failure(name + " is already defined as variable " + name, in);
              if (fInfo.contains(name)) return Failure(name + " is already defined as f-function " + name, in);
              
              // 2. fixed arity and constructor uniquness - also constructor global uniqueness
              val constructor = args.head.asInstanceOf[Constructor]
              val cName = constructor.name
              if (cInfo.contains(cName)){
                val cArity = cInfo(cName)
                if (cArity != constructor.args.size){
                  return Failure(cName + " is already defined as constructor with arity " + cArity, in);
                }
              }
              if (gInfo.contains(name)) {
                val (arity, cNames) = gInfo(name)
                if (arity != args.size){
                  return Failure(name + " is already defined as g-function with arity " + arity, in);
                }
                if (cNames.contains(cName)) {
                  return Failure("g-function " + name + " with constructor " + cName + " is already defined", in);
                }
                cNames + cName
              } else {
                gInfo(name) = (args.size, Set(cName))
              }
              // 3.a No variable occurs more than once in a left side
              // 3.b Name of variable must be unique in global context
              for (arg <- constructor.args ::: args.tail){
                val v = arg.asInstanceOf[Variable]
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
            } 
          }
          
          validateTerm(p.term) match {
            case None => return Success(p, in.rest)
            case Some(errorMsg) => return Failure(errorMsg, in)
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
  
  // parse term without context
  // so there is no call classification.
  def parseTerm(r: Reader[Char]): ParseResult[Term] = term(new lexical.Scanner(r))
  
}