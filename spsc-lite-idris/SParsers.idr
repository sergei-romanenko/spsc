module SParsers

import Text.Token
import Text.Lexer
import Text.Parser

import SLanguage

%default total

--
-- Tokens
--

data IdentKind
  = UId
  | LId
  | FId
  | GId

implementation Eq IdentKind where
  (==) UId UId = True
  (==) LId LId = True
  (==) FId FId = True
  (==) GId GId = True
  (==) _ _ = False

data SLLTokenKind
  = TkIdent IdentKind
  | TkPunct
  | TkIgnore

implementation Eq SLLTokenKind where
  (==) (TkIdent k1) (TkIdent k2) = k1 == k2
  (==) TkPunct TkPunct = True
  (==) TkIgnore TkIgnore = True
  (==) _ _ = False

SLLToken : Type
SLLToken = Token SLLTokenKind

TokenKind SLLTokenKind where
  TokType (TkIdent x) = String
  TokType TkPunct = ()
  TokType TkIgnore = ()

  tokValue (TkIdent k) x = x
  tokValue TkPunct x = ()
  tokValue TkIgnore x = ()

identifier : Lexer
identifier = alpha <+> many alphaNum

uId : Lexer
uId = expect upper <+> identifier

lId : Lexer
lId = expect lower <+> identifier

fId : Lexer
fId = expect (is 'f') <+> identifier

gId : Lexer
gId = expect (is 'g') <+> identifier


sllTokenMap : TokenMap SLLToken
sllTokenMap = toTokenMap $
  [ (spaces, TkIgnore)
  , (uId, TkIdent UId)
  , (fId, TkIdent FId)
  , (gId, TkIdent GId)
  , (lId, TkIdent LId)
  , (is '(', TkPunct)
  , (is ')', TkPunct)
  , (is ',', TkPunct)
  , (is '=', TkPunct)
  , (is ';', TkPunct)
  ]

lexSLL : String -> Maybe (List SLLToken)
lexSLL s
  = case lex sllTokenMap s of
         (tokens, _, _, "") => Just $ map TokenData.tok tokens
         _ => Nothing

--
-- SLL parser
--

uIdent : Grammar SLLToken True Name
uIdent = match (TkIdent UId)

fIdent : Grammar SLLToken True Name
fIdent = match (TkIdent FId)

gIdent : Grammar SLLToken True Name
gIdent = match (TkIdent GId)

lIdent : Grammar SLLToken True Name
lIdent =
  match (TkIdent FId) <|> match (TkIdent GId) <|> match (TkIdent LId)

symbol : String -> Grammar SLLToken True ()
symbol req = terminal (\t =>
  case t of
    (Tok TkPunct text) => if req == text then Just () else Nothing
    (Tok _ text) => Nothing)

parens : (p : Grammar SLLToken c a) -> Grammar SLLToken True a
parens p = between (symbol "(") (symbol ")") p

commaSep1 : (p : Grammar SLLToken True a) -> Grammar SLLToken True (List a)
commaSep1 p = sepBy1 (symbol ",") p

commaSep : (p : Grammar SLLToken True a) -> Grammar SLLToken False (List a)
commaSep p = sepBy (symbol ",") p

--
-- Programs
--

%default covering

mutual

  expression : Grammar SLLToken True Exp
  expression = constr <|> fCall <|> gCall <|> variable

  constr : Grammar SLLToken True Exp
  constr =
    do ctrName <- uIdent
       commit
       argList <- option [] (parens (commaSep expression))
       pure (Call Ctr ctrName argList)

  fCall : Grammar SLLToken True Exp
  fCall =
    do name <- fIdent
       argList <- parens (commaSep1 expression)
       pure (Call FCall name argList)

  gCall : Grammar SLLToken True Exp
  gCall =
    do name <- gIdent
       argList <- parens (commaSep1 expression)
       pure (Call GCall name argList)

  variable : Grammar SLLToken True Exp
  variable =
    do name <- lIdent
       pure (Var name)

fRule : Grammar SLLToken True Rule
fRule =
  do functionName <- fIdent
     commit
     paramList <- parens (commaSep1 lIdent)
     symbol "="
     ruleRhs <- expression
     symbol ";"
     pure (FRule functionName paramList ruleRhs)

  
gRule : Grammar SLLToken True Rule
gRule =
  do functionName <- gIdent
     commit
     symbol "("
     cname <- uIdent
     cparamList <- option [] (parens (commaSep lIdent))
     paramList <- many (symbol "," *> lIdent)
     symbol ")"; symbol "="
     ruleRhs <- expression
     symbol ";"
     pure (GRule functionName cname cparamList paramList ruleRhs)

rule : Grammar SLLToken True Rule
rule = fRule <|> gRule

program : Grammar SLLToken False Program
program =
  do ruleList <- many(rule)
     eof
     pure (MkProgram ruleList)

-- Parser

ignored : SLLToken -> Bool
ignored (Tok TkIgnore _) = True
ignored _ = False


parseStr : Grammar SLLToken c ast -> String -> Maybe ast
parseStr g input =
  case lexSLL input of
    Just toks =>
      case parse g $ filter (not . ignored) toks of
        Right (j, []) => Just j
        _ => Nothing
    Nothing => Nothing

export
parseExp : String -> Maybe Exp
parseExp input = parseStr expression input

export
parseProg : String -> Maybe Program
parseProg input = parseStr program input

{-
run :: Show a => Parser a -> String -> IO ()
run p input =
  case (parse p "" input) of
    Left err ->
      do putStr "parse error at "
         print err
    Right x  -> print x

parseSLL :: String -> IO ()
parseSLL = run program
-}
