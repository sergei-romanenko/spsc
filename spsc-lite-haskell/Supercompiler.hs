module Supercompiler where

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Control.Monad.State

import SLanguage
import ShowUtil
import SParsers
import Algebra
import MSG
import HE
import ProcessTree

lookupF prog name =
  let Program rules = prog in
  head [ (params, body) | FRule name' params body <- rules, name == name' ]

lookupGC :: Program -> Name -> Name -> (Params, Params, Exp)
lookupGC prog name cname =
  let Program rules = prog in
  head [ (cparams, params, body) |
    GRule name' cname' cparams params body <- rules,
    name == name',
    cname == cname' ]

lookupG :: Program -> Name -> [(Name, Params, Params, Exp)]
lookupG prog name =
  let Program rules = prog in
  [ (cname, cparams, params, body) |
    GRule name' cname cparams params body <- rules,
    name == name' ]

-- Driving

drivingStep :: Program -> Exp -> State Int [Branch]

drivingStep prog e =
  case e of
    Call Ctr name args ->
      return $ [ (arg, Nothing) | arg <- args  ]
    Call FCall name args ->
      let (params, body) = lookupF prog name
          p2a = Map.fromList(params `zip` args)
          body' = applySubst p2a body
      in return $ [(body', Nothing)]
    Call GCall name (Call Ctr cname cargs : args) ->
      let (cparams, params, body) = lookupGC prog name cname
          cp2ca = Map.fromList(cparams `zip` cargs)
          p2a = Map.fromList(params `zip` args)
          body' = applySubst (cp2ca `Map.union` p2a) body
      in return $ [(body', Nothing)]
    Call GCall name (Var vname : args) ->
      mapM (\(cname, cparams, params, _) ->
               driveBranch prog e vname cname cparams params)
           (lookupG prog name)
    Call GCall name (arg0 : args) ->
      do branches <- drivingStep prog arg0
         return $ [ (Call GCall name (e' : args), c) | (e', c) <- branches]
    Let body bindings ->
      return $
        (body, Nothing) : [(e', Nothing) | (_, e') <- bindings]

driveBranch :: Program -> Exp -> Name -> Name -> Params -> Params ->
               State Int (Exp, Maybe Contraction)

driveBranch prog e vname cname cparams params =
  do cparams' <- freshNameList (length cparams)
     let cargs = map Var cparams'
     let vname2ctr = Map.fromList[(vname, Call Ctr cname cargs)]
     let e' = applySubst vname2ctr e
     [(e'', Nothing)] <- drivingStep prog e'
     return $ (e'', Just $ Contraction vname cname cparams')

---- The parts common to the basic and advanced supercompilers.

findMoreGeneralAncestors :: Tree -> Node -> [Node]

findMoreGeneralAncestors tree beta@(Node _ eB _ _ _) =
  [ alpha  | alpha@(Node _ eA _ _ _) <- ancestors tree beta,
             isFGCall eA, eB `instOf` eA]

unprocessedNodes :: Tree -> [Node]

unprocessedNodes tree =
  let leaves = map (tree IntMap.!) (treeLeaves tree)
  in [ leaf | leaf <- leaves, not $ isProcessed tree leaf ]

buildLoop :: (Program -> Tree -> Node -> State Int Tree) ->
                        Program ->
                        Tree -> State Int Tree

buildLoop buildStep prog tree =
  case unprocessedNodes tree of
    [] -> return tree
    beta : _ ->
      do tree' <- buildStep prog tree beta
         buildLoop buildStep prog tree'

initTree :: Exp -> Tree

initTree e = IntMap.singleton 0 (Node 0 e Nothing Nothing [])

buildProcessTree :: (Program -> Tree -> Node -> State Int Tree) -> 
                    Program -> Exp -> Tree

buildProcessTree buildStep prog e =
  (evalState $
     buildLoop buildStep prog (initTree e))
    10000 

-- If beta `instOf` alpha, we generalize beta by introducing
-- a let-expression, in order to make beta the same as alpha
-- (modulo variable names).

loopBack :: Program -> Tree -> Node -> Node -> State Int Tree

loopBack prog tree beta@(Node bId eB _ _ _) alpha@(Node _ eA _ _ _) =
  do let Just subst = matchAgainst eA eB
         bindings = Map.toAscList subst
         letExp = Let eA bindings
         tree' = replaceSubtree tree bId letExp
     return tree'

-- This function applies a driving step to the node's expression,
-- and, in general, adds children to the node.

expandNode :: Program -> Tree -> Node -> State Int Tree

expandNode prog tree beta@(Node bId eB _ _ _) =
  do branches <- drivingStep prog eB
     tree' <- addChildren tree bId branches
     return tree'

---- Basic supercompiler

basic_buildStep :: Program -> Tree -> Node -> State Int Tree

basic_buildStep prog tree beta =
  case findMoreGeneralAncestors tree beta of
    [] -> expandNode prog tree beta
    alpha : _ -> loopBack prog tree beta alpha

basic_buildProcessTree :: Program -> Exp -> Tree

basic_buildProcessTree prog e =
  buildProcessTree basic_buildStep prog e

---- Advanced Supercompiler with homeomorphic imbedding and generalization  

abstract :: Tree -> Node -> Exp -> Subst -> State Int Tree

abstract tree alpha@(Node aId eA _ _ _) e subst =
  do let bindings = Map.toAscList subst
         letExp = Let e bindings
         tree' = replaceSubtree tree aId letExp
     return tree'

split :: Tree -> Node -> State Int Tree

split tree b@(Node nId e@(Call kind name args) c p chIds) =
  do names' <- freshNameList(length args)
     let letExp = Let (Call kind name (map Var names')) (names' `zip` args)
         tree' = replaceSubtree tree nId letExp
     return $ tree'

generalizeAlphaOrSplit tree
      beta@(Node _ eB _ _ _) alpha@(Node _ eA _ _ _) =
  do gen@(Gen e aSubst bSubst) <- msg eA eB
     case e of
       Var _ -> split tree beta
       _     -> abstract tree alpha e aSubst

findEmbeddedAncestors :: Tree -> Node -> [Node]

findEmbeddedAncestors tree beta@(Node _ eB _ _ _) =
  [ alpha  | alpha@(Node _ eA _ _ _) <- ancestors tree beta,
             isFGCall eA, eA `embeddedIn` eB]

advanced_buildStep :: Program -> Tree -> Node -> State Int Tree

advanced_buildStep prog tree beta =
  case findMoreGeneralAncestors tree beta of
    [] ->
      case findEmbeddedAncestors tree beta of
        []        -> expandNode prog tree beta
        alpha : _ -> generalizeAlphaOrSplit tree beta alpha
    alpha : _ -> loopBack prog tree beta alpha

advanced_buildProcessTree :: Program -> Exp -> Tree

advanced_buildProcessTree prog e =
  buildProcessTree advanced_buildStep prog e
