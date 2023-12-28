module PTBuilder

import Data.SortedMap
import Control.Monad.State

import SLanguage
import SParsers
import Algebra
import MSG
import HE
import ProcessTree

-- Looking up

lookupF : Program -> Name -> (Params, Exp)
lookupF (MkProgram rules) name =
  case the (List (Params, Exp))
       [ (params, body) | FRule name' params body <- rules, name == name' ] of
    pb :: _ => pb

lookupGC : Program -> Name -> Name -> (Params, Params, Exp)
lookupGC (MkProgram rules) name cname =
  case the (List (Params, Params, Exp))
    [ (cparams, params, body) | GRule name' cname' cparams params body <- rules,
      name == name', cname == cname' ] of
    cppb :: _ => cppb

lookupG : Program -> Name -> List (Name, Params, Params, Exp)
lookupG (MkProgram rules) name =
  [ (cname, cparams, params, body) |
    GRule name' cname cparams params body <- rules,
    name == name' ]

-- Applying contractions

applyContr : Maybe Contraction -> Exp -> Exp
applyContr Nothing e = e
applyContr (Just (MkContraction vname cname cparams)) e =
  let cargs = [Var vn | vn <- cparams]
      subst = the Subst $ fromList [(vname, Call Ctr cname cargs)]
  in applySubst subst e

-- Driving

mutual

  export
  drivingStep : Program -> Exp -> State Nat (List Branch)
  drivingStep prog e =
    case e of
      Call Ctr name args =>
        pure $ [ (arg, Nothing) | arg <- args  ]
      Call FCall name args =>
        let (params, body) = lookupF prog name
            p2a = the Subst $ fromList (params `zip` args)
            body' = applySubst p2a body
        in pure $ [(body', Nothing)]
      Call GCall name (Call Ctr cname cargs :: args) =>
        let (cparams, params, body) = lookupGC prog name cname
            pa = (cparams `zip` cargs) ++ (params `zip` args)
            subst = the Subst $ fromList pa
            body' = applySubst subst body
        in pure $ [(body', Nothing)]
      Call GCall name (Var vname :: args) =>
        for (lookupG prog name) (\(cname, cparams, params, _) =>
                 driveBranch prog e vname cname cparams params)
      Call GCall name (arg0 :: args) =>
        do branches <- drivingStep prog arg0
           pure $ [ (Call GCall name (e' ::
                      [ applyContr c arg | arg <- args ]), c) | (e', c) <- branches]
      _ => idris_crash "drivingStep: unexpected case"

  driveBranch : Program -> Exp -> Name -> Name -> Params -> Params ->
                State Nat Branch
  driveBranch prog e vname cname cparams params =
    do cparams' <- freshNameList (length cparams)
       let cargs = map Var cparams'
       let vname2ctr = insert vname (Call Ctr cname cargs) empty
       let e' = applySubst vname2ctr e
       [(e'', Nothing)] <- drivingStep prog e'
       pure $ (e'', Just $ MkContraction vname cname cparams')

---- The parts common to the basic and advanced tree builders.

isMoreGeneral : Node -> Node -> Bool
isMoreGeneral beta alpha =
  let eB = nodeExp beta
      eA = nodeExp alpha
  in isFGCall eA && (eB `instOf` eA)

findAMoreGeneralAncestor : Tree -> Node -> Maybe Node
findAMoreGeneralAncestor tree beta =
  findAncestor (isMoreGeneral beta) tree beta

export
findAnUnprocessedNode : Tree -> Maybe Node
findAnUnprocessedNode tree =
  let leaves = map (getNode tree) (treeLeaves tree) in
  let nodes = [ leaf | leaf <- leaves, not $ isProcessed tree leaf ] in
  case nodes of
    [] => Nothing
    alpha :: _ => Just alpha

public export
BuildStep : Type
BuildStep = Program -> Tree -> Node -> State Nat Tree

public export
BuildLoop : Type
BuildLoop = BuildStep -> Program -> Tree -> State Nat Tree

public export
TreeBuilder : Type
TreeBuilder = Program -> Exp -> Tree

buildLoop : BuildLoop
buildLoop buildStep prog tree =
  case findAnUnprocessedNode tree of
    Nothing => pure tree
    Just beta =>
      do tree' <- buildStep prog tree beta
         buildLoop buildStep prog tree'

initTree : Exp -> Tree
initTree e = insert 0 (MkNode 0 e Nothing Nothing [] Nothing) empty

export
mkTreeBuilder : BuildLoop -> BuildStep -> TreeBuilder
mkTreeBuilder loop step prog e =
  (evalState $ loop step prog (initTree e)) 10000

-- This function replaces the expression in a node with
-- a let-expression, and then adds childe nodes.
-- Thus, a let-node cannot be a leaf.

decomposeNode : Tree -> NodeId -> Exp -> Bindings -> State Nat Tree
decomposeNode tree nId e bindings =
  do let branches = (e, Nothing) ::
           [ (exp, Nothing) | (name, exp) <- bindings ]
     let tree' = replaceSubtree tree nId (Let e bindings)
     addChildren tree' nId branches

-- If beta `instOf` alpha, we generalize beta by introducing
-- a let-expression, in order to make beta the same as alpha
-- (modulo variable names).

generalizeNode : Program -> Tree -> Node -> Node -> State Nat Tree
generalizeNode prog tree beta@(MkNode bId eB _ _ _ _)
                         alpha@(MkNode _ eA _ _ _ _) =
  do let Just subst = matchAgainst eA eB
     let bindings = toList subst
     decomposeNode tree bId eA bindings

-- This function applies a driving step to the node's expression,
-- and, in general, adds children to the node.

driveNode : Program -> Tree -> Node -> State Nat Tree
driveNode prog tree beta@(MkNode bId eB _ _ _ _) =
  do branches <- drivingStep prog eB
     addChildren tree bId branches

---- Basic tree builder

export
basicBuildStep : BuildStep
basicBuildStep prog tree beta =
  case findFuncAncestor tree beta of
    Just alpha =>
      pure $ setBack tree beta (nodeId alpha)
    Nothing =>
      case findAMoreGeneralAncestor tree beta of
        Just alpha => generalizeNode prog tree beta alpha
        Nothing => driveNode prog tree beta

export
basicBuilder : TreeBuilder
basicBuilder prog e =
  mkTreeBuilder buildLoop basicBuildStep prog e

---- Advanced tree builder with homeomorphic imbedding and generalization  

abstract : Tree -> Node -> Exp -> Subst -> State Nat Tree
abstract tree alpha@(MkNode aId eA _ _ _ _) e subst =
  decomposeNode tree aId e (toList subst)

split : Tree -> Node -> State Nat Tree
split tree b@(MkNode nId e@(Call kind name args) c p chIds back) =
  do names' <- freshNameList (length args)
     let e = Call kind name (map Var names')
     let bindings = names' `zip` args
     decomposeNode tree nId e bindings

generalizeAlphaOrSplit : Tree -> Node -> Node -> State Nat Tree
generalizeAlphaOrSplit tree beta@(MkNode _ eB _ _ _ _)
                            alpha@(MkNode _ eA _ _ _ _) =
  do MkGen e aSubst bSubst <- msg eA eB
     case e of
       Var _ => split tree beta
       _     => abstract tree alpha e aSubst

isEmbeddedAncestor : Node -> Node -> Bool
isEmbeddedAncestor beta alpha =
  let eB = nodeExp beta
      eA = nodeExp alpha
  in isFGCall eA && (eA `embeddedIn` eB)

findAnEmbeddedAncestor : Tree -> Node -> Maybe Node
findAnEmbeddedAncestor tree beta =
  findAncestor (isEmbeddedAncestor beta) tree beta

export
advancedBuildStep : BuildStep
advancedBuildStep prog tree beta =
  case findFuncAncestor tree beta of
    Just alpha =>
      pure $ setBack tree beta (nodeId alpha)
    Nothing =>
      case findAMoreGeneralAncestor tree beta of
        Just alpha => generalizeNode prog tree beta alpha
        Nothing =>
          case findAnEmbeddedAncestor tree beta of
            Just alpha => generalizeAlphaOrSplit tree beta alpha
            Nothing => driveNode prog tree beta

export
advancedBuilder : TreeBuilder
advancedBuilder prog e =
  mkTreeBuilder buildLoop advancedBuildStep prog e
