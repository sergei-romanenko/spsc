import SPSC.Basic
import SPSC.Algebra
import SPSC.SLanguage
import SPSC.ProcessTree

import Batteries
import Std.Data.TreeMap

open Std

open CKind Exp

--

structure Sig where
  name : Name
  params : List Name
deriving Inhabited

abbrev Sigs := TreeMap Nat Sig
abbrev GenState := Sigs × (List FRule × List GRule)

def isVarTest (tree : Tree) (b : Node) : Bool :=
  match b.children with
    | [] => false
    | bChId :: _=>
        (getNode tree bChId).contr.isSome

def isFuncNode (fIds : List NodeId) (nId : NodeId) : Bool :=
  fIds.contains nId

def getFGSig (pref : String) (nId : NodeId)
      (name : Name) (vs : List Name) : StateM GenState Sig := do
  let (sigs, rules) ← get
  match sigs.get? nId with
    | none => do
        let name' := s!"{pref}{name.drop 1}{sigs.size + 1}"
        let sig' := Sig.mk name' vs
        let sigs' := sigs.insert nId sig'
        set $ (sigs', rules)
        pure sig'
    | some sig' =>
        pure sig'

def putFRule (newRule : FRule) : StateM GenState Unit := do
  let (sigs, (fRules, gRules)) ← get
  set (sigs, (fRules ++ [ newRule ], gRules))
  pure ()

def putGRules (newRules : List GRule) : StateM GenState Unit := do
  let (sigs, (fRules, gRules)) ← get
  set (sigs, (fRules, gRules ++ newRules))
  pure ()

def getChContr (tree : Tree) (nIds : List NodeId) : List (Name × List Name) :=
  let children := nIds.map (getNode tree)
  children.flatMap (fun node =>
    if let some c := node.contr then [(c.cname, c.cparams)] else [])

mutual

  partial
  def genResExp (tree : Tree) (fIds : List NodeId) (b : Node) :
        StateM GenState Exp :=
    match b.back with
      | none => do
          match b.exp with
            | Var _ => pure b.exp
            |  Call Ctr cname _ => do
                let es ← genResExps tree fIds b.children
                pure $ Call Ctr cname es
            | Call FCall name args =>
                genResCall tree fIds b name
            | Call GCall name args =>
                genResCall tree fIds b name
            | Let _ bs =>
                let chNode := getNode tree b.children.head!
                let e' ← genResExp tree fIds chNode
                let es' ← genResExps tree fIds b.children.tail!
                let vnames := List.map Prod.fst bs
                let subst := TreeMap.ofList (vnames.zip es')
                pure $ applySubst subst e'
      | some aId => do
          let a := getNode tree aId
          let (sigs, rules) <- get
          let sa := sigs.get! aId
          let args := sa.params.map Var
          let subst := (b.exp.matchWith a.exp).get!
          let aChNode := getNode tree a.children.head!
          match aChNode.contr with
            | none =>
                pure $ applySubst subst (Call FCall sa.name args)
            | some _ =>
                pure $ applySubst subst (Call GCall sa.name args)

  partial
  def genResExps (tree : Tree) (fIds nIds : List NodeId) :
        StateM GenState (List Exp) := do
    let ns := nIds.map (getNode tree)
    ns.mapM (genResExp tree fIds)

  partial
  def genResCall (tree : Tree) (fIds : List NodeId) (b : Node)
        (name : Name) : StateM GenState Exp :=
    let params := b.exp.vars
    if isVarTest tree b then do
      let s <- getFGSig "g" b.id name params
      let bodies <- genResExps tree fIds b.children
      let contrs := getChContr tree b.children
      let gRules := (contrs.zip bodies).map
            (fun ((cname', cparams'), body') =>
              GRule.mk s.name cname' cparams' params.tail! body')
      putGRules gRules
      pure $ Call GCall s.name (params.map Var)
    else if isFuncNode fIds b.id then do
      let s <- getFGSig "f" b.id name params
      let bChNode := getNode tree b.children.head!
      let body' <- genResExp tree fIds bChNode
      putFRule $ FRule.mk s.name s.params body'
      pure $ Call FCall s.name (params.map Var)
    else
      let bChNode := getNode tree b.children.head!
      genResExp tree fIds bChNode

end /- mutual -/

def genResidualProgram' (tree : Tree) : StateM GenState (Exp × Program) := do
  let initNode := getNode tree 0
  let fIds := funcNodeIds tree
  let resExp ← genResExp tree fIds initNode
  let (_, (fRules, gRules)) ← get
  pure (resExp, Program.mk fRules gRules)

def genResidualProgram (tree : Tree) : (Exp × Program) :=
  (genResidualProgram' tree (∅, ([], []))).fst
