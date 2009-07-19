module SLanguage where

type Name = String
type Arg = Exp
type Args = [Arg]
type Params = [Name]

data CKind = Ctr | FCall | GCall
  deriving (Eq)

data Exp
  = Var Name
  | Call CKind Name Args
      deriving Eq

data Pat = Pat Name Params

data Rule
  = FRule Name Params Exp
  | GRule Name Pat Params Exp

data Program = Program [Rule]
