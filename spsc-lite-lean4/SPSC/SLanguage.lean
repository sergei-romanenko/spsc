--- Exp ---

abbrev Name := String
abbrev Params := List Name

inductive CKind where
  | Ctr | FCall | GCall
deriving BEq, Repr

open CKind

inductive Exp where
  | Var : Name -> Exp
  | Call : CKind -> Name -> List Exp -> Exp
  | Let : Exp -> List (Name × Exp) -> Exp
deriving BEq, Repr, Inhabited

abbrev Arg := Exp
abbrev Args := List Arg
abbrev Bindings := List (Name × Exp)

structure FRule where
  name : Name
  params : Params
  body : Exp
deriving BEq, Repr, Inhabited

structure GRule where
  name : Name
  cname : Name
  cparams : Params
  params : Params
  body : Exp
deriving BEq, Repr, Inhabited

def Rule := Sum FRule GRule

structure Program where
  fRules : List FRule
  gRules : List GRule
deriving BEq, Repr

structure SLLTask where
  exp : Exp
  program : Program
deriving BEq, Repr

open Exp

--- toString ---

mutual

  private
  def tsParams : List String -> String
    | [] => ""
    | x :: xs => x ++ tsParamsTail xs

  private
  def tsParamsTail : List String -> String
    | [] => ""
    | xs => "," ++ tsParams xs

end

def tsPat : String -> List String -> String
  | cname, [] => cname
  | cname, cparams =>
      cname ++ "(" ++ tsParams cparams ++ ")"

mutual

  private
  def tsArgs : List Exp -> String
    | [] => "()"
    | x :: xs => "(" ++ tsExp x ++ tsArgsTail xs ++ ")"

  private
  def tsArgsTail : List Exp -> String
    | [] => ""
    | x :: xs => "," ++ tsExp x ++ tsArgsTail xs

  def tsBindings : List (String × Exp) -> String
    | [] => ""
    | (v, e) :: xs => v ++ "=" ++ tsExp e ++ tsBindingsTail xs

  private
  def tsBindingsTail : List (String × Exp) -> String
    | [] => ""
    | xs => "," ++ tsBindings xs

  def tsExp : Exp -> String
    | Var name => name
    | Call Ctr name [] => name
    | Call _ name args => name ++ tsArgs args
    | Let e bindings => "let " ++ tsBindings bindings ++ " in " ++ tsExp e

end

instance : ToString Exp where
  toString := tsExp

instance : ToString FRule where
  toString := fun f =>
    s!"{f.name}({tsParams f.params})={f.body};"

instance : ToString GRule where
  toString := fun g =>
    s!"{g.name}({tsPat g.cname g.cparams}{tsParamsTail g.params})={g.body};"

instance : ToString Program where
  toString := fun program =>
    String.join (List.map toString program.fRules)
    ++
    String.join (List.map toString program.gRules)

instance : ToString SLLTask where
  toString := fun t =>
    s!"{t.exp} where {t.program}"
