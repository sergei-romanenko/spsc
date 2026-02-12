import Parser

import SPSC.SLanguage

open CKind Exp Program SLLTask

-- SLL parser monad

abbrev SLLParser := SimpleParser String.Slice Char

def runSLLParser {α} (p : SLLParser α) (input : String) : Option α :=
  match p.run input.toSlice with
  | .ok _ x => some x
  | .error _ _ => none

namespace SLLParser
open Parser Char

-- Tokens

def ws_char : SLLParser Char :=
  withErrorMessage "ws_char" do
    char ' ' <|> char '\n' <|> char '\r' <|> char '\t'

def ws : SLLParser Unit :=
  withErrorMessage "ws" do
    dropMany ws_char

def ws1 : SLLParser Unit :=
  withErrorMessage "ws1" do
    dropMany1 ws_char

def letter : SLLParser Char :=
  withErrorMessage "letter" do
    ASCII.alpha

def identTailChar : SLLParser Char :=
  withErrorMessage "identTailChar" do
    ASCII.alphanum

def identTail : SLLParser String :=
  withErrorMessage "identTail" do
    foldl String.push "" identTailChar

def identifier : SLLParser String :=
  withErrorMessage "aIdent" do
    let a ← letter
    let s ← identTail
    pure (a.toString ++ s)

def uIdent : SLLParser String :=
  withErrorMessage "uIdent" do
    let _ ← lookAhead ASCII.uppercase
    let  name <- identifier
    pure name

def lIdent : SLLParser String :=
  withErrorMessage "lIdent" do
    let _ ← lookAhead ASCII.lowercase
    let  name <- identifier
    pure name

def fIdent : SLLParser String :=
  withErrorMessage "fIdent" do
    let _ ← lookAhead (char 'f')
    let  name <- identifier
    pure name

def gIdent : SLLParser String :=
  withErrorMessage "gIdent" do
    let _ ← lookAhead (char 'g')
    let  name <- identifier
    pure name

def ch_ws (c : Char) : SLLParser Char :=
  withErrorMessage "ch_ws" do
    char c <* ws

def ws_ch_ws (c : Char) : SLLParser Char :=
  withErrorMessage "ws_ch" do
    ws *> char c <* ws

def ws_ch (c : Char) : SLLParser Char :=
  withErrorMessage "ws_ch_ws" do
    ws *> char c

def parens {α} (p : SLLParser α) : SLLParser α :=
  withErrorMessage "parens" do
    ch_ws '(' *> p <* ws_ch ')'

def commaSep {α} (p : SLLParser α) : SLLParser (List α) :=
  withErrorMessage "commaSep" do
    let xs ← sepBy (ws_ch_ws ',') p
    pure xs.toList

def commaSep1 {α} (p : SLLParser α) : SLLParser (List α) :=
  withErrorMessage "commaSep1" do
    let xs ← sepBy1 (ws_ch_ws ',') p
    pure xs.toList

-- Programs

mutual

partial
def expression : SLLParser Exp :=
  first [ constructor, fCall, gCall, var ]

partial
def constructor : SLLParser Exp := do
  let ctrName <- uIdent
  let argList <- option! (parens (commaSep expression))
  pure (Call Ctr ctrName argList)

partial
def fCall : SLLParser Exp := do
  let name ← fIdent
  let argList <- parens (commaSep1 expression)
  pure (Call FCall name argList)

partial
def gCall : SLLParser Exp := do
  let name ← gIdent
  let argList ← parens (commaSep1 expression)
  pure (Call GCall name argList)

partial
def var : SLLParser Exp := do
  let name ← lIdent
  pure (Var name)

end /- mutual -/

partial
def expression_eoi : SLLParser Exp :=
  expression <* endOfInput

def pattern : SLLParser (Name × Params) :=
  withErrorMessage "pattern" do
    let constructorName ← uIdent
    let variableList <- option! (parens (commaSep lIdent))
    pure (constructorName, variableList)

def fRule : SLLParser Rule := do
  let functionName ← fIdent
  let params ← parens (commaSep1 lIdent)
  let _ ← ws_ch_ws '='
  let ruleRhs ← expression
  let _ ← ws_ch ';'
  pure $ Sum.inl $ FRule.mk functionName params ruleRhs

def gRule : SLLParser Rule := do
  let functionName ← gIdent
  let _ ← ch_ws '('
  let (cname, cparams) <- pattern
  let params <- takeMany (ws_ch_ws ',' *> lIdent)
  let _ ← ws_ch ')'
  let _ ← ws_ch_ws '='
  let ruleRhs <- expression
  let _ ← ws_ch ';'
  pure $ Sum.inr $ GRule.mk functionName cname cparams params.toList ruleRhs

def rule : SLLParser Rule :=
  ws *> first [
    fRule,
    gRule
  ]

def program : SLLParser Program := do
  let rules ← takeMany rule
  let ruleList := rules.toList
  let _ ← ws
  let fRules : List FRule := ruleList.foldr
    (fun r fs => if let Sum.inl f := r then f :: fs else fs) []
  let gRules : List GRule := ruleList.foldr
    (fun r gs => if let Sum.inr g := r then g :: gs else gs) []
  pure (Program.mk fRules gRules)

def program_eoi : SLLParser Program :=
  program <* endOfInput

-- Supercompilation tasks

def task : SLLParser SLLTask := do
  let e ← expression
  let _ ← ws1 *> chars "where" <* ws1
  let p <- program
  pure (SLLTask.mk e p)

def task_eoi : SLLParser SLLTask :=
  task <* endOfInput

end SLLParser

-- Runners

def parseExp (input : String) : Option Exp :=
  runSLLParser SLLParser.expression_eoi input

def parseProg (input : String) : Option Program :=
  runSLLParser SLLParser.program_eoi input

def parseTask (input : String) : Option SLLTask :=
  runSLLParser SLLParser.task_eoi input
