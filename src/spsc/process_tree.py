'''
Created on Aug 17, 2009

@author: Sergei Romanenko
'''

from sll_language import *
from algebra import *

class Contraction(object):
    def __init__(self, vname, cname, cparams):
        self.vname = vname
        self.cname = cname
        self,cparams = cparams

    def __str__(self):
        cparams_s = ",".join(self.cparams)
        pat_s = self.cname
        if len(self.cparams) > 0 :
            pat_s += "(" + cparams_s + ")"
        return self.vname + " = " + pat_s

#type Branch = (Exp, Optional(Contraction))

#data Node = Node
#  { nodeId  ::NodeId
#  , nodeExp ::Exp
#  , nodeContr   ::Maybe Contraction
#  , nodeParent  ::(Maybe NodeId)
#  , nodeChildren::[NodeId]
#  }
#    deriving Show

def showNodeId(node):
    if node :
        return node.id
    else:
        return None

class Node(object):
    def __init__(self, exp, contr, parent, children):
        self.exp = exp
        self.contr = contr
        self.parent = parent
        self.children = children

    def ancestors(self):
        n = self.parent
        while n:
            yield n
            n = n.parent

    def __str__(self):
        return "Node{%s [%s] %s}" % (exp, contr, showNodeId(parent))

#-- By convention, the root node's id is 0.
#type Tree = IntMap.IntMap Node
#
#ancestors :: Tree -> Node -> [Node]
#
#ancestors tree node =
#  case nodeParent node of
#    Nothing -> []
#    Just parentId ->
#      let parentNode = tree IntMap.! parentId in
#      parentNode : ancestors tree parentNode

#isProcessed :: Tree -> Node -> Bool
#
#isProcessed tree node =
#  case nodeExp node of
#    Var _ -> True
#    Call Ctr name args -> null args
#    Call _ _ _ ->
#      case funcAncestors tree node of
#        [] -> False
#        _ : _ -> True
#    Let _ _ -> False
#
#equivCall :: Exp -> Exp -> Bool
#
#equivCall e e' =
#  isFGCall e' && e `equiv` e'
#
#funcAncestors :: Tree -> Node -> [Node]
#
#funcAncestors tree node =
#  [node' | node' <- ancestors tree node,
#           nodeExp node `equiv` nodeExp node' ]
#
#funcNodes :: Tree -> [Node]
#
#funcNodes tree =
#  do leafId <- treeLeaves tree
#     let node = tree IntMap.! leafId
#     funcAncestors tree node
#
#isFuncNode :: Tree -> NodeId -> Bool
#
#isFuncNode tree nId =
#  nId `elem` [ nId' | Node nId' _ _ _ _ <- funcNodes tree]
#
#addChildren :: Tree -> NodeId -> [Branch] -> State Int Tree
#
#addChildren tree nId branches =
#  do let Node _ e c p chIds = tree IntMap.! nId
#     chIds' <- freshNodeIdList (length branches)
#     let tree' = IntMap.insert nId (Node nId e c p (chIds++chIds')) tree
#         chNodes = [ Node nId' e' c' (Just nId) [] |
#                       (nId', (e', c')) <- chIds' `zip` branches]
#         tree'' = tree' `IntMap.union` IntMap.fromList (chIds' `zip` chNodes)
#     return $ tree'' 
#
#freshNodeId :: State Int NodeId
#
#freshNodeId =
#  do t <- get
#     put $ t+1
#     return $ t
#
#freshNodeIdList :: (MonadState a m, Num a, Enum a) => a -> m [a]
#
#freshNodeIdList n =
#  do t <- get
#     put $ t + n
#     return $ [t..(t+n-1)]
#
#replaceSubtree :: Tree -> NodeId -> Exp -> Tree
#
#replaceSubtree tree nId e' =
#  let Node _ e c p chIds = tree IntMap.! nId
#      tree' = foldl (flip IntMap.delete) tree chIds
#  in IntMap.insert nId (Node nId e' c p []) tree'
#
#treeLeaves :: IntMap.IntMap Node -> [NodeId]
#
#treeLeaves tree = treeLeavesAcc tree 0 []
#
#treeLeavesAcc :: Tree -> NodeId -> [NodeId] -> [NodeId]
#
#treeLeavesAcc tree nId acc =
#  let Node _ _ _ _ children = tree IntMap.! nId
#  in if null children
#     then nId : acc
#     else foldr (treeLeavesAcc tree) acc children

class ProcessTree(object):
    def __init__(self, root=None):
        self.root = root
        
    def __str__(self):
        return "ProcessTree: %s %s" % (showNodeId(self.root), self.children)
