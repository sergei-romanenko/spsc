module SLanguageShow(show) where

import SLanguage

showParams [] = ""
showParams (x : xs) = x ++ showParamsTail xs

showParamsTail [] = ""
showParamsTail (x : xs) = "," ++ x ++ showParamsTail xs

showArgs [] = "()"
showArgs (x : xs) = "(" ++ show x ++ showArgsTail xs ++ ")"

showArgsTail [] = ""
showArgsTail (x : xs) = "," ++ show x ++ showArgsTail xs

instance Show Exp where
  show (Var name) = name
  show (Call Ctr name []) = name
  show (Call _ name args) = name ++ showArgs args

instance Show Pattern where
  show (Pattern name []) = name
  show (Pattern name params) =
    name ++ "(" ++ showParams params ++ ")"

instance Show Rule where
  show (FRule name params expression) =
    name ++ "(" ++ showParams params ++ ")=" ++ show expression
  show (GRule name pattern params expression) =
    name ++ "(" ++ show pattern ++ showParamsTail params ++ ")=" ++ show expression

instance Show Program where
  show (Program rules) = concat[show rule ++ ";" | rule <- rules]
    