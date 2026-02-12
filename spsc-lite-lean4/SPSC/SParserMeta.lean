-- Using Lean 4's metaprogramming facilities.

import SPSC.SLanguage
import Batteries

import Lean
import Std

open Lean
open Elab
open Meta

-- Exp

declare_syntax_cat exp

syntax ident "(" (exp),* ")" : exp
syntax ident : exp

def String.inspectIdent (p : Char -> Bool) (i : String) : Bool :=
  match String.Pos.Raw.get? i ⟨ 0 ⟩ with
    | none => false
    | some c => p c

partial
def mkExp : Syntax -> Except String Exp
  | `(exp| $i:ident( $[$es:exp],* ) ) => do
      let i := i.getId.toString
      let es ← es.mapM mkExp
      let es := es.toList
      if i.inspectIdent Char.isUpper then
        pure $ Exp.Call CKind.Ctr i es
      else if es.length == 0 then
        throw "mkExp"
      else if i.inspectIdent (· == 'f') then
        pure $ Exp.Call CKind.FCall i es
      else if i.inspectIdent (· == 'g') then
        pure $ Exp.Call CKind.GCall i es
      else throw "mkExp"
  | `(exp| $i:ident) => do
      let i := i.getId.toString
      if i.inspectIdent Char.isUpper then
        pure $ Exp.Call CKind.Ctr i []
      else if i.inspectIdent Char.isLower then
        pure $ Exp.Var i
      else throw "mkExp"

  | _ => throw "mkExp"

def ckindToExp :CKind -> Expr
  | .Ctr => mkConst ``CKind.Ctr
  | .FCall => mkConst ``CKind.FCall
  | .GCall => mkConst ``CKind.GCall

def expToM : Exp -> MetaM Expr
  | .Var v => mkAppM ``Exp.Var #[mkStrLit v]
  | .Call kind name args => do
      let args ← args.mapM expToM
      let args ← mkListLit (mkConst ``Exp) args
      mkAppM ``Exp.Call #[ckindToExp kind, mkStrLit name, args]
  | _ => throwUnsupportedSyntax

def exExpToTerm : Except String Exp -> MetaM Expr
  | .error _ => throwUnsupportedSyntax
  | .ok e => expToM e

elab "test_mkExp " "[exp|" e:exp "]" : term => exExpToTerm $ mkExp e

#reduce test_mkExp [exp| abc ]
#reduce test_mkExp [exp| C ]
#reduce test_mkExp [exp| C ( ) ]
#reduce test_mkExp [exp| C(x) ]
#reduce test_mkExp [exp| C(x, X()) ]
#reduce test_mkExp [exp| f(x,y) ]
#reduce test_mkExp [exp| g(x) ]


-- Rule

declare_syntax_cat params
declare_syntax_cat tail
declare_syntax_cat rule
declare_syntax_cat program

syntax (ident),* : params
syntax ("," ident)* : tail

syntax ident "(" ident tail ")" "=" exp ";" : rule
syntax ident "(" ident "(" params ")" tail ")" "=" exp ";" : rule

syntax "`[params| " params "]" : term
syntax "`[tail| " tail "]" : term

#check_failure `[params| ]
#check_failure `[params| x ]
#check_failure `[params| x, y ]
#check_failure `[tail| ]
#check_failure `[tail| , x ]
#check_failure `[tail| , x, y ]

syntax "`[rule| " rule "]" : term

#check_failure `[rule| f(x) = x; ]
#check_failure `[rule| f(x, y) = x; ]
#check_failure `[rule| g(C) = x; ]
#check_failure `[rule| g(C(p, q), y) = x; ]
#check_failure `[rule| g(C, y) = x; ]
#check_failure `[rule| g(C(p, q), y) = x; ]

partial
def elabIdents (xs : Array (TSyntax `ident)) : Except String (List String) := do
  let xs := xs.map (fun x => x.getId.toString)
  if xs.all (String.inspectIdent Char.isLower) then
    pure $ xs.toList
  else throw "elabIdents"

partial
def elabParams : Syntax -> Except String (List String)
  | `(params| $[$xs:ident],* ) => do
      elabIdents xs
  | _ => throw "elabParams"

partial
def elabTail : Syntax -> Except String (List String)
  | `(tail| $[, $xs:ident]* ) => do
      elabIdents xs
  | _ => throw "elabTail"

partial
def mkRule : Syntax -> Except String (FRule ⊕ GRule)
  | `(rule| $f:ident ( $c:ident $xs:tail ) = $e:exp ; ) => do
      let f := f.getId.toString
      let c := c.getId.toString
      let xs ← elabTail xs
      let e ← mkExp e
      if f.inspectIdent (· == 'f') && c.inspectIdent Char.isLower then
        pure $ Sum.inl $ FRule.mk f (c :: xs) e
      else if f.inspectIdent (· == 'g') && c.inspectIdent Char.isUpper then
        pure $ Sum.inr $ GRule.mk f c [] xs e
      else
        throw "mkRule"
  | `(rule| $g:ident ( $c:ident ($ps:params) $xs:tail ) = $e:exp ; ) => do
      let g := g.getId.toString
      let c := c.getId.toString
      let xs ← elabTail xs
      let e ← mkExp e
      if g.inspectIdent (· == 'g') && c.inspectIdent Char.isUpper then
        let ps ← elabParams ps
        pure $ Sum.inr $ GRule.mk g c ps xs e
      else
        throw "mkRule"
  | _ => throw "mkRule"

def fRuleToM (r : FRule) : MetaM Expr := do
  let params := r.params.map mkStrLit
  let params ← mkListLit (mkConst ``String) params
  let e ← expToM r.body
  mkAppM ``FRule.mk #[mkStrLit r.name, params, e]

def gRuleToM (r : GRule) : MetaM Expr := do
  let params := r.params.map mkStrLit
  let params ← mkListLit (mkConst ``String) params
  let cparams := r.cparams.map mkStrLit
  let cparams ← mkListLit (mkConst ``String) cparams
  let e ← expToM r.body
  mkAppM ``GRule.mk #[mkStrLit r.name, mkStrLit r.cname, cparams, params, e]

def ruleToM (r : FRule ⊕ GRule) : MetaM Expr := do
  match r with
  | .inl fRule =>
      mkAppOptM ``Sum.inl #[none, some (mkConst ``GRule), ← fRuleToM fRule]
  | .inr gRule =>
      mkAppOptM ``Sum.inr #[some (mkConst ``FRule), none, ← gRuleToM gRule]

def exRuleToTerm : Except String Rule -> MetaM Expr
  | .error _ => throwUnsupportedSyntax
  | .ok e => ruleToM e

elab "test_mkRule " "[rule|" e:rule "]" : term => exRuleToTerm $ mkRule e

#reduce test_mkRule [rule| f(x) = x; ]
#reduce test_mkRule [rule| g(C) = X; ]

-- Program

syntax (rule)* : program

syntax "`[program| " program "]" : term

#check_failure `[program| f(x) = x; g(C, y) = x; ]

partial
def mkProgram : Syntax -> Except String Program
  | `(program| $[$rs:rule]* ) => do
      let rs ← rs.mapM mkRule
      let (fRules, gRules) := rs.toList.partitionMap id
      pure $ Program.mk fRules gRules
  | _ => throw "mkProgram"

def programToM (r : Program) : MetaM Expr := do
  let f ← r.fRules.mapM fRuleToM
  let g ← r.gRules.mapM gRuleToM
  let f ← mkListLit (mkConst ``FRule) f
  let g ← mkListLit (mkConst ``GRule) g
  mkAppM ``Program.mk #[f, g]

def exProgramToTerm : Except String Program -> MetaM Expr
  | .error _ => throwUnsupportedSyntax
  | .ok p => programToM p

elab "test_mkProgram " "[program|" p:program "]" : term => exProgramToTerm $ mkProgram p

#reduce test_mkProgram [program| f(x) = x; g(C) = X; ]

-- Task

declare_syntax_cat task

syntax exp " where " program : task

syntax "`[task| " task "]" : term

#check_failure `[task| f(a) where f(x) = x; g(C, y) = x; ]

partial
def mkTask : Syntax -> Except String SLLTask
  | `(task| $e:exp where $p:program ) => do
      let e ← mkExp e
      let p ← mkProgram p
      pure $ SLLTask.mk e p
  | _ => throw "mkTask"

def taskToM (t : SLLTask) : MetaM Expr := do
  let e ← expToM t.exp
  let p ← programToM t.program
  mkAppM ``SLLTask.mk #[e, p]

def exTaskToTerm : Except String SLLTask -> MetaM Expr
  | .error _ => throwUnsupportedSyntax
  | .ok t => taskToM t
