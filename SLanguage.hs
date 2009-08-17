module SLanguage where

import ShowUtil

type Name = String
type Arg = Exp
type Args = [Arg]
type Params = [Name]

data CKind = Ctr | FCall | GCall
  deriving (Eq)

data Exp
  = Var Name
  | Call CKind Name Args
  | Let Exp [(Name, Exp)]
      deriving Eq

data Rule
  = FRule Name Params Exp
  | GRule Name Name Params Params Exp

data Program = Program [Rule]

--- Show ---

instance Show Exp where
  show (Var name) = name
  show (Call Ctr name []) = name
  show (Call _ name args) = name ++ showArgs args
  show (Let e bindings) = "let " ++ showBindings bindings ++ " in " ++ show e

instance Show Rule where
  show (FRule name params expression) =
    name ++ "(" ++ showParams params ++ ")=" ++ show expression
  show (GRule name cname cparams params expression) =
    name ++ "(" ++ showPat cname cparams ++ showParamsTail params ++ ")=" ++ show expression

instance Show Program where
  show (Program rules) = concat[show rule ++ ";" | rule <- rules]

