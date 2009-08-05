module ProcessTree where

import qualified Data.IntMap as IntMap
import Control.Monad.State

import SLanguage
import Algebra

type NodeId = Int

data Contraction = Contraction Name Name Params

type Branch = (Exp, Maybe Contraction)

data Node = Node
  { nodeId  ::NodeId
  , nodeExp ::Exp
  , contr   ::Maybe Contraction
  , parent  ::(Maybe NodeId)
  , children::[NodeId]
  }

-- By convention, the root node's id is 0.
type Tree = IntMap.IntMap Node

ancestors :: Tree -> Node -> [Node]

ancestors tree node =
  case parent node of
    Nothing -> []
    Just parentId ->
      let parentNode = tree IntMap.! parentId in
      parentNode : ancestors tree parentNode

isProcessed tree node =
  case nodeExp node of
    Var _ -> True
    Call Ctr name args -> null args
    Call _ _ _ ->
      case funcNode tree node of
        Nothing -> False
        Just _ -> True

equivCall e e' =
  isFGCall e' && e `equiv` e'

funcNode tree node =
  case [node' | node' <- ancestors tree node,
                nodeExp node `equiv` nodeExp node' ] of
    [] -> Nothing
    (node' : _) -> Just node'

addChildren :: Tree -> NodeId -> [Branch] -> State Int Tree

addChildren tree nId branches =
  do let Node _ e c p chIds = tree IntMap.! nId
     chIds' <- freshNodeIdList (length branches)
     let tree' = IntMap.insert nId (Node nId e c p (chIds++chIds')) tree
         chNodes = [ Node nId' e' c' (Just nId) [] | (nId', (e', c')) <- chIds' `zip` branches]
         tree'' = tree' `IntMap.union` IntMap.fromList (chIds `zip` chNodes)
     return $ tree'' 

freshNodeId :: State Int NodeId
freshNodeId =
  do t <- get
     put $ t+1
     return $ t

freshNodeIdList :: (MonadState NodeId m) => NodeId -> m [NodeId]

freshNodeIdList n =
  do t <- get
     put $ t + n
     return $ [t..(t+n-1)]

--  def replace(n: Node, exp: Term) = 
--    if (n == root) new Tree(n, Map().withDefaultValue(Nil))
--    else {
--      val p = n.parent
--      val cs = children(p) map {m => if (m == n) new Node(exp, p, n.contr) else m}
--      new Tree(root, children + (n -> cs))
--    }
--  
--  def leaves_(node: Node): List[Node] = 
--    if (children(node).isEmpty) List(node) 
--    else List.flatten(children(node) map leaves_)
--  
--  def leaves() = leaves_(root)
--}