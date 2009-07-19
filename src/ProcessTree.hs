module ProcessTree where

import qualified Data.IntMap as IntMap

import SLanguage
import Algebra

type Key = Int

data Contraction = Contraction Name Pat
data Node = Node {key::Key, expr::Exp, parent::(Maybe Key), contr::Contraction}

-- By convention, the root node's key is 0.
type Tree = IntMap.IntMap Node

ancestors :: Tree -> Node -> [Node]

ancestors tree node =
  case parent node of
    Nothing -> []
    Just parentKey ->
      let parentNode = tree IntMap.! parentKey in
      parentNode : ancestors tree parentNode

isProcessed tree node =
  case expr node of
    Var _ -> True
    Call Ctr name args -> null args
    Call _ _ _ ->
      case funcNode tree node of
        Nothing -> False
        Just _ -> True

--equivCall expr expr' =
--  isFGCall expr' && expr `equiv` expr'

funcNode tree node =
  case [node' | node' <- ancestors tree node, expr node `equiv` expr node' ] of
    [] -> Nothing
    (node' : _) -> Just node'

--class Node(val expr: Term, val parent: Node, val contr: Contraction) {
--  
--  
--  def fnode =
--    ancestors.find{n => !trivial(n.expr) && equiv(expr, n.expr)}.getOrElse(null)
--}
--
--class Tree(val root: Node, val children: Map[Node, List[Node]]) {
--  
--  def addChildren(n: Node, cs: List[(Term, Contraction)]) = 
--    new Tree(root, children + (n -> (cs map {case (t, b) => new Node(t, n, b)})))
--
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