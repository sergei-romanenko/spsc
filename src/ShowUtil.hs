module ShowUtil(show) where

import SLanguage
import ProcessTree

showParams [] = ""
showParams (x : xs) = x ++ showParamsTail xs

showParamsTail [] = ""
showParamsTail (x : xs) = "," ++ x ++ showParamsTail xs

showArgs [] = "()"
showArgs (x : xs) = "(" ++ show x ++ showArgsTail xs ++ ")"

showArgsTail [] = ""
showArgsTail (x : xs) = "," ++ show x ++ showArgsTail xs

showPat cname [] = cname
showPat cname cparams =
  cname ++ "(" ++ showParams cparams ++ ")"

instance Show Exp where
  show (Var name) = name
  show (Call Ctr name []) = name
  show (Call _ name args) = name ++ showArgs args

instance Show Rule where
  show (FRule name params expression) =
    name ++ "(" ++ showParams params ++ ")=" ++ show expression
  show (GRule name cname cparams params expression) =
    name ++ "(" ++ showPat cname cparams ++ showParamsTail params ++ ")=" ++ show expression

instance Show Program where
  show (Program rules) = concat[show rule ++ ";" | rule <- rules]

instance Show Contraction where
  show (Contraction vname cname cparams) =
    vname ++ " = " ++ showPat cname cparams 
    