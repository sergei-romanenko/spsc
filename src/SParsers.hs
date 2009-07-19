module SParsers where

import Char

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
--import Text.ParserCombinators.Parsec.Expr

import SLanguage
import SLanguageShow

---------
-- Tokens
---------

tokenDefs = emptyDef
  { commentStart = "{-"
  , commentEnd   = "-}"
  , identStart = letter
  , identLetter = alphaNum
--  , opStart = oneOf "~&=:"
--  , opLetter = oneOf "~&=:"
--  , reservedOpNames = ["~", "&", "=", ":="]
--  , reservedNames = ["true", "false", "nop",
--                     "if", "then", "else", "fi",
--                     "while", "do", "od"]
  }

lexer = P.makeTokenParser tokenDefs

parens          = P.parens lexer    
braces          = P.braces lexer    
semiSep         = P.semiSep lexer  
semiSep1        = P.semiSep1 lexer    
commaSep        = P.commaSep lexer
commaSep1       = P.commaSep1 lexer
brackets        = P.brackets lexer
whiteSpace      = P.whiteSpace lexer    
symbol          = P.symbol lexer    
identifier      = P.identifier lexer    
reserved        = P.reserved lexer    
reservedOp      = P.reservedOp lexer
integer         = P.integer lexer    
charLiteral     = P.charLiteral lexer    
stringLiteral   = P.stringLiteral lexer

uIdent =
  do lookAhead upper
     name <- identifier
     return name

lIdent =
  do lookAhead lower
     name <- identifier
     return name

fIdent =
  do lookAhead (char 'f')
     name <- identifier
     return name

gIdent =
  do lookAhead (char 'g')
     name <- identifier
     return name

-----------
-- Programs
-----------

program :: Parser Program

program =
  do ruleList <- many(rule)
     eof
     return (Program ruleList)

rule = fRule <|> gRule

fRule =
  do functionName <- fIdent
     paramList <- parens (commaSep1 lIdent)
     symbol "="
     ruleRhs <- expression
     symbol ";"
     return (FRule functionName paramList ruleRhs)

gRule =
  do functionName <- gIdent
     symbol "("
     pat <- pattern
     paramList <- many( symbol "," >> lIdent)
     symbol ")"
     symbol "="
     ruleRhs <- expression
     symbol ";"
     return (GRule functionName pat paramList ruleRhs)

pattern :: Parser Pat

pattern =
  do constructorName <- uIdent
     variableList <- option [] (parens( commaSep lIdent ))
     return (Pat constructorName variableList)

expression =   constructor <|> variableOrFunctionCall

constructor =
  do ctrName <- uIdent 
     argList <- option [] (parens (commaSep expression))
     return (Call Ctr ctrName argList)

variableOrFunctionCall =
  do name <- lIdent
     do argList <- parens (commaSep1 expression)
        case head name of
          'f' -> return (Call FCall name argList)
          'g' -> return (Call GCall name argList)
        <|> return (Var name)

run :: Show a => Parser a -> String -> IO ()
run p input =
  case (parse p "" input) of
    Left err ->
      do putStr "parse error at "
         print err
    Right x  -> print x
    
parseSLL :: String -> IO ()
parseSLL = run program

pProg input =
  case parse program "" input of
    Right x -> x

pExp input =
  case parse expression "" input of
    Right x -> x
