import SPSC.Basic
import SPSC.SLanguage
import SPSC.SParser
import SPSC.Algebra
import SPSC.MSG
import SPSC.HE
import SPSC.ProcessTree

import Batteries
import Std.Data.TreeMap

open Std

open CKind Exp

-- Looking up

def lookupF (program : Program) (name : Name) : FRule :=
  program.fRules.filter (name == ·.name) |> List.head!

def lookupGC (program : Program) (name : Name) (cname : Name) : GRule :=
  program.gRules.filter
    (fun g => name == g.name && cname == g.cname) |> List.head!

def lookupG (program : Program) (name : Name) : List GRule :=
  program.gRules.filter (name == ·.name)

-- Applying contractions

def applyContr  : Option Contraction -> Exp -> Exp
  | none, e => e
  | some c, e =>
      let cargs := c.cparams.map Var
      let subst : Subst := { (c.vname, Call Ctr c.cname cargs) }
      applySubst subst e

-- Driving

mutual

  partial
  def drivingStep (p : Program) (e : Exp) : StateM Nat (List Branch) :=
    match e with
      | Call Ctr name args =>
          pure $ args.map (Branch.mk · none)
      | Call FCall name args => do
          let f := lookupF p name
          let p2a : Subst := TreeMap.ofList (f.params.zip args)
          let body' := applySubst p2a f.body
          pure $ [Branch.mk body' none]
      | Call GCall name (Call Ctr cname cargs :: args) => do
          let g := lookupGC p name cname
          let pa := (g.cparams.zip cargs) ++ (g.params.zip args)
          let subst : Subst := TreeMap.ofList pa
          let body' := applySubst subst g.body
          pure $ [Branch.mk body' none]
      | Call GCall name (Var vname :: args) =>
          lookupG p name |> List.mapM (driveBranch p e vname)
      | Call GCall name (arg0 :: args) => do
          let branches ← drivingStep p arg0
          pure $ branches.map (fun b =>
            let es := args.map (applyContr b.contr?)
            Branch.mk (Call GCall name (b.exp :: es)) b.contr?)

      | _ => panic! "drivingStep: unexpected case"

  partial
  def driveBranch (p : Program) (e : Exp)
      (vname : Name) (g : GRule) : StateM Nat Branch := do
    let cparams' ← freshNameList g.cparams.length
    let cargs := cparams'.map Var
    let vname2ctr : Subst := { (vname, Call Ctr g.cname cargs) }
    let e' := applySubst vname2ctr e
    let bs ← drivingStep p e'
    let e'' := bs.head!.exp
    pure $ Branch.mk e'' (some $ Contraction.mk vname g.cname cparams')

end /- mutual -/

---- The parts common to the basic and advanced tree builders.

def isMoreGeneral (b a : Node) : Bool :=
  a.exp.isFGCall && b.exp.instOf a.exp

def findAMoreGeneralAncestor (tree : Tree) (beta : Node) : Option Node :=
  findAncestor (isMoreGeneral beta) tree beta

def findAnUnprocessedNode (tree : Tree) : Option Node :=
  let leaves := (treeLeaves tree).map (getNode tree)
  let nodes : List Node := do
    let leaf ← leaves
    guard (!isProcessed leaf)
    pure leaf
  nodes.head?

def BuildStep := Program -> Tree -> Node -> StateM Nat Tree

def BuildLoop := BuildStep -> Program -> Tree -> StateM Nat Tree

def TreeBuilder := Program -> Exp -> Tree

partial
def buildLoop : BuildLoop := fun buildStep prog tree =>
  match findAnUnprocessedNode tree with
    | none => pure tree
    | some beta => do
        let tree' ← buildStep prog tree beta
        buildLoop buildStep prog tree'

def initTree (e : Exp) : Tree :=
  {(0, (Node.mk 0 e none none [] none))}

def mkTreeBuilder (loop : BuildLoop) (step : BuildStep) : TreeBuilder :=
  fun prog e =>
    loop step prog (initTree e) 10000 |> Prod.fst

-- This function replaces the expression in a node with
-- a let-expression, and then adds childe nodes.
-- Thus, a let-node cannot be a leaf.

def decomposeNode (tree : Tree) (nId : NodeId) (e : Exp)
    (bindings : Bindings) : StateM Nat Tree := do
  let branches : List Branch := Branch.mk e none ::
        bindings.map (fun (_, exp) => Branch.mk exp none)
  let tree' := replaceSubtree tree nId (Let e bindings)
  addChildren tree' nId branches

-- If beta.instOf alpha, we generalize beta by introducing
-- a let-expression, in order to make beta the same as alpha
-- (modulo variable names).

def generalizeNode (tree : Tree) (beta alpha : Node) : StateM Nat Tree :=
  let subst := (beta.exp.matchWith alpha.exp).get!
  let bindings := subst.toList
  decomposeNode tree beta.id alpha.exp bindings

-- This function applies a driving step to the node's expression,
-- and, in general, adds children to the node.

def driveNode (prog : Program) (tree : Tree) (beta : Node) : StateM Nat Tree := do
  let branches ← drivingStep prog beta.exp
  addChildren tree beta.id branches

---- Basic tree builder

def basicBuildStep : BuildStep := fun prog tree beta =>
  match findFuncAncestor tree beta with
    | some alpha =>
        pure $ setBack tree beta alpha.id
    | none =>
        match findAMoreGeneralAncestor tree beta with
          | some alpha => generalizeNode tree beta alpha
          | none => driveNode prog tree beta

def basicBuilder : TreeBuilder := fun prog e =>
  mkTreeBuilder buildLoop basicBuildStep prog e

---- Advanced tree builder with homeomorphic imbedding and generalization

def abstract (tree : Tree) (alpha : Node) (e : Exp) (subst : Subst)
    : StateM Nat Tree :=
  decomposeNode tree alpha.id e subst.toList

def split (tree : Tree) (b : Node) : StateM Nat Tree :=
  match b.exp with
    | Call kind name args => do
        let names' ← freshNameList args.length
        let e := Call kind name (names'.map Var)
        let bindings := names'.zip args
        decomposeNode tree b.id e bindings
    | _ => panic! "split"

def generalizeAlphaOrSplit (tree : Tree) (b a : Node) : StateM Nat Tree := do
  let g ← msg a.exp b.exp
  match g.e with
    | Var _ => split tree b
    | _     => abstract tree a g.e g.a

def isEmbeddedAncestor (b a : Node) : Bool :=
  a.exp.isFGCall && a.exp.embeddedIn b.exp

def findAnEmbeddedAncestor (tree : Tree) (beta : Node) : Option Node :=
  findAncestor (isEmbeddedAncestor beta) tree beta

def advancedBuildStep : BuildStep := fun prog tree beta =>
  match findFuncAncestor tree beta with
    | some alpha =>
        pure $ setBack tree beta alpha.id
    | none =>
        match findAMoreGeneralAncestor tree beta with
          | some alpha => generalizeNode tree beta alpha
          | none =>
              match findAnEmbeddedAncestor tree beta with
                | some alpha => generalizeAlphaOrSplit tree beta alpha
                | none => driveNode prog tree beta

def advancedBuilder : TreeBuilder := fun prog e =>
  mkTreeBuilder buildLoop advancedBuildStep prog e
