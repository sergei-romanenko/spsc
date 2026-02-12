import SPSC.SLanguage

import Batteries
import Std.Data.TreeMap

open Std

open CKind Exp

-- Substitutions

abbrev Subst := TreeMap Name Exp

instance : BEq Subst where
  beq s1 s2 := s1.toList == s2.toList

def theSameFunctor : Exp -> Exp -> Bool
  | Call kind1 name1 _, Call kind2 name2 _ =>
      kind1 == kind2 && name1 == name2
  | _, _ => false

def applySubst (m : Subst) (e : Exp) : Exp :=
  match e with
    | Var name =>
        m.getD name e
    | Call kind name args =>
        Call kind name (List.map (applySubst m) args)
    | Let _ _ =>
        Inhabited.default

-- Matching

mutual

  def matchAgainstAcc : Option Subst → Exp → Exp → Option Subst
    | some m, Var vname, e' =>
        match m.get? vname with
          | none => some $ m.insert vname e'
          | some e'' =>
            if e' != e'' then none else some m
    | some m, Call kind name args, Call kind' name' args' =>
        if kind == kind' && name == name'
          then matchAgainstAccL (some m) args args' else none
    | _, _, _ => none

  def matchAgainstAccL : Option Subst → Args → Args → Option Subst
    | some m, [], [] => some m
    | some m, e :: es, e' :: es' =>
        matchAgainstAccL (matchAgainstAcc (some m) e e') es es'
    | _, _, _ => none

end /- mutual -/

def Exp.matchWith (e : Exp) (p : Exp) : Option Subst :=
  matchAgainstAcc (some ∅) p e

def Exp.instOf (e : Exp) (p : Exp) : Bool :=
  match matchWith e p with
    | none => false
    | some _ => true

def Exp.equiv (e : Exp) (e' : Exp) : Bool :=
  e.instOf e' && e'.instOf e

-- Free variables

def Exp.vars : Exp -> List Name
  | Var vname => [vname]
  | Call _ _ args =>
      args.flatMap Exp.vars
  | Let _ _ => []

-- Function calls

def Exp.isFGCall : Exp -> Bool
  | Call FCall _ _ => true
  | Call GCall _ _ => true
  | _ => false

-- Fresh names

def mkName (k : Nat) : String :=
  s!"v{k}"

def freshName : StateM Nat Name := do
  let k ← get
  set (k + 1)
  pure $ mkName k

def freshNameList (n : Nat) : StateM Nat (List Name) := do
  let k ← get
  set (k + n)
  pure $ if n == 0 then [] else List.map mkName (List.range' k n)
