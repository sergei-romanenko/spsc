import SPSC.Basic
import SPSC.SLanguage
import SPSC.Algebra

import Batteries
import Std.Data.TreeMap

open Std

open CKind Exp

abbrev NodeId := Nat

structure Contraction where
  vname : Name
  cname : Name
  cparams : Params

open Contraction

def tsContraction (c : Contraction) : String :=
  s!"{c.vname} = {tsPat c.cname c.cparams}"

instance : ToString Contraction where
  toString := tsContraction

structure Branch where
  exp : Exp
  contr? : Option Contraction
deriving Inhabited

def tsBranch (b : Branch) : String :=
  s!"({b.exp}, \
      {if let some c := b.contr? then "some (" ++ tsContraction c ++ ")" else "none" })"

instance : ToString Branch where
  toString := tsBranch

structure Node where
  id : NodeId
  exp : Exp
  contr : Option Contraction
  parent : Option NodeId
  children : List NodeId
  back : Option NodeId
deriving Inhabited

def tsO {α} [ToString α] (d : String) (v : Option α) :=
  match v with
    | none => d
    | some x => toString x

def tsNode (n : Node) :=
s!"\{\
{n.id}^{tsO "_" n.parent }\
{tsO "" ((("^^" ++ ·) ∘ toString) <$> n.back)}\
: {n.exp}\
{tsO "" (((" ?" ++ ·) ∘ toString) <$> n.contr)}\
{if n.children.isEmpty then "" else (" @" ++ toString n.children)}\
}"

instance : ToString Node where
  toString := tsNode

-- By convention, the root node's id is 0.

abbrev Tree := TreeMap NodeId Node

def tsTree (tree : Tree) : String :=
  toString $ tree.toList.map Prod.snd

def getNode (tree : Tree) (nId : NodeId) : Node :=
  tree.get! nId

partial
def treeLeavesAcc (tree : Tree) (nId : NodeId) (acc : List NodeId) : List NodeId :=
  let children := (getNode tree nId).children
  if children.isEmpty
  then nId :: acc
  else children.foldr (treeLeavesAcc tree) acc

def treeLeaves (tree : Tree) : List NodeId :=
  treeLeavesAcc tree 0 []

def getParent (tree : Tree) (node : Node) : Option Node :=
  getNode tree <$> node.parent

def setBack (tree : Tree) (node : Node) (backId : NodeId) : Tree :=
  tree.insert node.id { node with back := some backId }

partial
def ancestors (tree : Tree) (node : Node) : List Node :=
  match getParent tree node with
    | none => []
    | some parentNode =>
        parentNode :: ancestors tree parentNode

partial
def findAncestor (p : Node -> Bool) (tree : Tree) (node : Node) : Option Node :=
  match getParent tree node with
    | none => none
    | some parentNode =>
        if p parentNode then some parentNode
                        else findAncestor p tree parentNode

def findFuncAncestor (tree : Tree) (node : Node) : Option Node :=
  findAncestor (fun node' => node.exp.equiv node'.exp ) tree node

def funcNodeIds (tree : Tree) : List NodeId := do
  (treeLeaves tree).flatMap (fun leafId =>
    if let some backId := (getNode tree leafId).back
    then [backId] else [])

def isProcessed (node : Node) : Bool :=
  match node.exp with
    | Var _ => true
    | Call Ctr _ args => args.isEmpty
    | Call _ _ _ => node.back.isSome
    | Let _ _ => false

partial
def deleteSubtree (tree : Tree) (nId : NodeId) : Tree :=
  let chIds := (getNode tree nId).children
  let tree' := chIds.foldl deleteSubtree tree
  tree'.erase nId

def replaceSubtree (tree : Tree) (nId : NodeId) (e' : Exp) : Tree :=
  let node := getNode tree nId
  let tree' := node.children.foldl deleteSubtree tree
  tree'.insert nId {node with exp := e', children := []}

def freshNodeId : StateM Nat NodeId := do
  let k ← get
  set (k + 1)
  pure k

def freshNodeIdList (n : Nat) : StateM Nat (List Nat) := do
  let k ← get
  set (k + n)
  pure $ List.range' k n

def addChildren (tree : Tree) (nId : NodeId) (branches : List Branch)
    : StateM Nat Tree :=
  let node := getNode tree nId
  do
    let chIds' ← freshNodeIdList branches.length
    let node' := {node with children := node.children ++ chIds'}
    let tree' := tree.insert nId node'
    let chNodes : List Node :=
      (chIds'.zip branches).map
        -- (fun (nId', (e', c')) => Node.mk nId' e' c' (some nId) [] none )
        (fun (nId', b) =>
          Node.mk nId' b.exp b.contr? (some nId) [] none )
    let tree'' := (chIds'.zip chNodes).foldl (fun acc (k, v) => acc.insert k v) tree'
    pure $ tree''
