import SPSC.Basic
import SPSC.SLanguage
import SPSC.Algebra

import Batteries
import Std.Data.TreeMap

open Std

open CKind Exp

structure Gen where
  e : Exp
  a : Subst
  b : Subst
deriving BEq

def tsGen (g : Gen) : String :=
  toString g.e ++ " =>> "
    ++ "{" ++ tsBindings g.a.toList ++ "}"
    ++ "{" ++ tsBindings g.b.toList ++ "}"

instance : ToString Gen where
  toString := tsGen

def mergeableKeyPairs (m1 m2 : Subst) : List (Name × Name) := do
  let ⟨k1, e1⟩ ← m1.toList
  let ⟨k2, e2⟩ ← m1.toList
  guard (k1 < k2)
  guard (e1 == e2)
  guard (m2.get? k1 == m2.get? k2   )
  pure (k1, k2)

def mergeSubexp (g : Gen) : Gen :=
  match mergeableKeyPairs g.a g.b with
    | [] => g
    | (k1, k2) :: _ =>
        let e := applySubst {(k1, Var k2)} g.e
        let a := g.a.erase k1
        let b := g.b.erase k1
        Gen.mk e a b

def mergeableFunctors (g : Gen) : List (Name × Exp) := do
  let ⟨k, e1⟩ ← g.a.toList
  let e2 := g.b.getD k Inhabited.default
  guard (theSameFunctor e1 e2)
  pure (k, e1)

def commonFunctor (g : Gen) : StateM Nat Gen :=
  match mergeableFunctors g with
    | [] => pure g
    | (k, e1) :: _ =>
      match e1, g.b.get? k with
      | Call ctr name args1, some (Call _ _ args2) => do
          let ns <- freshNameList args1.length
          let vs := ns.map Var
          let e' := applySubst {(k, Call ctr name vs)} g.e
          let na1 := ns.zip args1
          let na2 := ns.zip args2
          let a := na1.foldl (fun m ⟨n, a⟩ => m.insert n a) (g.a.erase k)
          let b := na2.foldl (fun m ⟨n, a⟩ => m.insert n a) (g.b.erase k)
          pure $ Gen.mk e' a b
      | _, _ => pure g

partial
def msgLoop (g : Gen) : StateM Nat Gen := do
  let g2 ← commonFunctor (mergeSubexp g)
  if g2 == g then pure g else msgLoop g2

def msg (e1 e2 : Exp) : StateM Nat Gen := do
  let k ← freshName
  let u := Gen.mk (Var k) {(k, e1)} {(k, e2)}
  msgLoop u
