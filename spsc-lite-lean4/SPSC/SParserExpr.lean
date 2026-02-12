-- Using Lean 4's metaprogramming facilities.

import SPSC.SLanguage

import Lean

open Lean Elab Meta

-- Exp

declare_syntax_cat exp

syntax ident "(" (exp),* ")" : exp
syntax ident : exp

def String.inspectIdent (p : Char -> Bool) (i : String) : Bool :=
  match String.Pos.Raw.get? i ⟨ 0 ⟩ with
    | none => false
    | some c => p c

partial
def elabExp : Syntax -> MetaM Expr
  | `(exp| $i:ident( $[$es:exp],* ) ) => do
      let i := i.getId.toString
      let es ← es.mapM elabExp
      let es ← mkListLit (mkConst ``Exp) es.toList
      if i.inspectIdent Char.isUpper then
        mkAppM ``Exp.Call #[mkConst ``CKind.Ctr, mkStrLit i, es]
      else if i.length == 0 then
        throwUnsupportedSyntax
      else if i.inspectIdent (· == 'f') then
        mkAppM ``Exp.Call #[mkConst ``CKind.FCall, mkStrLit i, es]
      else if i.inspectIdent (· == 'g') then
        mkAppM ``Exp.Call #[mkConst ``CKind.GCall, mkStrLit i, es]
      else throwUnsupportedSyntax
  | `(exp| $i:ident) => do
      let i := i.getId.toString
      let es ← mkListLit (mkConst ``Exp) []
      if i.inspectIdent Char.isUpper then
        mkAppM ``Exp.Call #[mkConst ``CKind.Ctr, mkStrLit i, es]
      else if i.inspectIdent Char.isLower then
        mkAppM ``Exp.Var #[mkStrLit i]
      else throwUnsupportedSyntax

  | _ => throwUnsupportedSyntax

elab "test_elabExp " "[exp|" e:exp "]" : term => elabExp e

#reduce test_elabExp [exp| abc ]
#reduce test_elabExp [exp| C ]
#reduce test_elabExp [exp| C ( ) ]
#reduce test_elabExp [exp| C(x) ]
#reduce test_elabExp [exp| C(x, X()) ]
#reduce test_elabExp [exp| f(x,y) ]
#reduce test_elabExp [exp| f(x) ]

-- Rule

declare_syntax_cat params
declare_syntax_cat tail
declare_syntax_cat rule

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
def elabIdents (xs : Array (TSyntax `ident)) : MetaM Expr := do
  let xs := xs.map (fun x => x.getId.toString)
  let ns := xs.map (mkStrLit ·)
  if xs.all (String.inspectIdent Char.isLower) then
    mkListLit (mkConst ``String) ns.toList
  else throwUnsupportedSyntax

partial
def elabParams : Syntax -> MetaM Expr
  | `(params| $[$xs:ident],* ) => do
      elabIdents xs
  | _ => throwUnsupportedSyntax

elab "test_elabParams " "[params|" xs:params "]" : term => elabParams xs

#reduce test_elabParams [params| ]
#reduce test_elabParams [params| x ]
#reduce test_elabParams [params| x, y ]

partial
def elabTail : Syntax -> MetaM Expr
  | `(tail| $[, $xs:ident]* ) => do
      elabIdents xs
  | _ => throwUnsupportedSyntax

elab "test_elabTail " "[tail|" xs:tail "]" : term => elabTail xs

#reduce test_elabTail [tail| ]
#reduce test_elabTail [tail| , x ]
#reduce test_elabTail [tail| , x, y ]

partial
def elabRule : Syntax -> MetaM (Expr ⊕ Expr)
  | `(rule| $f:ident ( $c:ident $xs:tail ) = $e:exp ; ) => do
      let f := f.getId.toString
      let c := c.getId.toString
      let xs ← elabTail xs
      let e ← elabExp e
      if f.inspectIdent (· == 'f') && c.inspectIdent Char.isLower then
        let cxs ← mkAppM ``List.cons #[mkStrLit c, xs]
        let fRule ← mkAppM ``FRule.mk #[mkStrLit f, cxs, e]
        -- let rule ← mkAppOptM ``Sum.inl #[none, some (mkConst ``GRule), fRule]
        pure $ Sum.inl fRule
      else if f.inspectIdent (· == 'g') && c.inspectIdent Char.isUpper then
        let ps ← mkListLit (mkConst ``String) []
        let gRule ← mkAppM ``GRule.mk #[mkStrLit f, mkStrLit c, ps, xs, e]
        -- let rule ← mkAppOptM ``Sum.inr #[some (mkConst ``FRule), none, gRule]
        pure $ Sum.inr gRule
      else
        throwUnsupportedSyntax
  | `(rule| $g:ident ( $c:ident ($ps:params) $xs:tail ) = $e:exp ; ) => do
      let g := g.getId.toString
      let c := c.getId.toString
      let xs ← elabTail xs
      let e ← elabExp e
      if g.inspectIdent (· == 'g') && c.inspectIdent Char.isUpper then
        let ps ← elabParams ps
        let gRule ← mkAppM ``GRule.mk #[mkStrLit g, mkStrLit c, ps, xs, e]
        -- let rule ← mkAppOptM ``Sum.inr #[some (mkConst ``FRule), none, gRule]
        pure $ Sum.inr gRule
      else
        throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

def runElabRule (ctx : Syntax) : MetaM Expr := do
  let fgRule ← elabRule ctx
  match fgRule with
      | .inl fRule => mkAppOptM ``Sum.inl #[none, some (mkConst ``GRule), fRule]
      | .inr gRule => mkAppOptM ``Sum.inr #[some (mkConst ``FRule), none, gRule]

elab "test_elabRule " "[rule|" r:rule "]" : term => runElabRule r

#reduce test_elabRule [rule| f(x, y) = x; ]
#reduce test_elabRule [rule| g(C, y) = y; ]
#reduce test_elabRule [rule| g(C(), y) = x; ]
#reduce test_elabRule [rule| g(C(x), y) = x; ]

-- Program

declare_syntax_cat program

syntax (rule)* : program
syntax "`[program| " program "]" : term

#check_failure `[program| f(x) = x; g(C(p,q), z) = q; ]

partial
def elabProgram : Syntax -> MetaM Expr
  | `(program| $[$rs:rule]* ) => do
      let rs ← rs.mapM elabRule
      let fRules : List Expr := rs.toList.flatMap
        (fun r => if let Sum.inl f := r then [f] else [])
      let fRules ← mkListLit (mkConst ``FRule) fRules
      let gRules : List Expr := rs.toList.flatMap
        (fun r => if let Sum.inr g := r then [g] else [])
      let gRules ← mkListLit (mkConst ``GRule) gRules
      -- pure $ mkStrLit "*"
      mkAppM ``Program.mk #[fRules, gRules]
  | _ => throwUnsupportedSyntax

elab "test_elabProgram " "[program|" p:program "]" : term => elabProgram p

#reduce test_elabProgram [program| f(x) = x; g(C(p,q), z) = q; ]

-- Task

declare_syntax_cat task
syntax exp " where " program : task
syntax "`[task| " task "]" : term

#check_failure `[task| f(a) where f(x) = x; g(C(p,q), z) = q; ]

partial
def elabTask : Syntax -> MetaM Expr
  | `(task| $e:exp where $p:program  ) => do
      let e ← elabExp e
      let p ← elabProgram p
      mkAppM ``SLLTask.mk #[e, p]
  | _ => throwUnsupportedSyntax

elab "test_elabTask " "[task|" p:task "]" : term => elabTask p

#reduce test_elabTask [task| f(a) where f(x) = x; g(C(p,q), z) = q; ]
