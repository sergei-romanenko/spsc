module ProcessTree

import Data.SortedMap
import Control.Monad.State

import SLanguage
import Algebra

%access public export

NodeId : Type
NodeId = Nat

data Contraction = MkContraction Name Name Params

implementation Show Contraction where
  show (MkContraction vname cname cparams) =
    vname ++ " = " ++ showPat cname cparams 

Branch : Type
Branch = (Exp, Maybe Contraction)

record Node where
  constructor MkNode
  nodeId : NodeId
  nodeExp : Exp
  nodeContr : Maybe Contraction
  nodeParent : Maybe NodeId
  nodeChildren : List NodeId

implementation Show Node where
  show (MkNode n exp contr parent children) =
    "{" ++ show n ++ "^" ++ maybe "_" show parent ++
    ": " ++ show exp ++
    maybe "" ((" ?" ++) . show) contr ++
    (if isNil children then "" else (" @" ++ show children)) ++ "}"

-- By convention, the root node's id is 0.

Tree : Type
Tree = SortedMap Nat Node

implementation Show Tree where
  show tree = show $ map snd $ toList tree

getNode : Tree -> Nat -> Node
getNode tree nId =
  case lookup nId tree of
    Nothing => idris_crash "getNode"
    Just node => node

treeLeavesAcc : Tree -> NodeId -> List NodeId -> List NodeId
treeLeavesAcc tree nId acc =
  let children = nodeChildren $ getNode tree nId
  in if isNil children
     then nId :: acc
     else foldr (treeLeavesAcc tree) acc children

treeLeaves : Tree -> List NodeId
treeLeaves tree = treeLeavesAcc tree 0 []

getParent : Tree -> Node -> Maybe Node
getParent tree node =
  getNode tree <$> nodeParent node

ancestors : Tree -> Node -> List Node
ancestors tree node =
  case getParent tree node of
    Nothing => []
    Just parentNode =>
      parentNode :: ancestors tree parentNode

findAncestor : (Node -> Bool) -> Tree -> Node -> Maybe Node
findAncestor p tree node =
  case getParent tree node of
    Nothing => Nothing
    Just parentNode =>
      if p parentNode then Just parentNode
                      else findAncestor p tree parentNode

findFuncAncestor : Tree -> Node -> Maybe Node
findFuncAncestor tree node =
  findAncestor (\node' => nodeExp node `equiv` nodeExp node') tree node

funcAncestors : Tree -> Node -> List Node
funcAncestors tree node =
  [node' | node' <- ancestors tree node,
           nodeExp node `equiv` nodeExp node' ]

funcNodes : Tree -> List Node
funcNodes tree =
  do leafId <- treeLeaves tree
     funcAncestors tree $ getNode tree leafId

isFuncNode : Tree -> NodeId -> Bool
isFuncNode tree nId =
  nId `elem` [ nId' | MkNode nId' _ _ _ _ <- funcNodes tree]

isProcessed : Tree -> Node -> Bool
isProcessed tree node =
  case nodeExp node of
    Var _ => True
    Call Ctr name args => isNil args
    Call _ _ _ => isJust $ findFuncAncestor tree node
    Let _ _ => False

equivCall : Exp -> Exp -> Bool
equivCall e e' =
  isFGCall e' && (e `equiv` e')

replaceSubtree : Tree -> NodeId -> Exp -> Tree

replaceSubtree tree nId e' =
  let MkNode _ e c p chIds = getNode tree nId
      tree' = foldl (flip delete) tree chIds
  in insert nId (MkNode nId e' c p []) tree'


freshNodeId : State Nat NodeId
freshNodeId =
  do k <- get
     put $ S k
     pure $ k

freshNodeIdList : Nat -> State Nat (List Nat)
freshNodeIdList n =
  do k <- get
     put $ n + k
     pure $ [k .. pred (n + k)]

addChildren : Tree -> NodeId -> List Branch -> State Nat Tree
addChildren tree nId branches =
  let MkNode _ e c p chIds = getNode tree nId in
  do chIds' <- freshNodeIdList (length branches)
     let tree' = insert nId (MkNode nId e c p (chIds ++ chIds')) tree
     let chNodes = [ MkNode nId' e' c' (Just nId) [] |
                       (nId', (e', c')) <- chIds' `zip` branches]
     let tree'' = insertFrom (chIds' `zip` chNodes) tree'
     pure $ tree'' 
