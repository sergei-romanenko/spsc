module ResProgGen

import Data.SortedMap
import Control.Monad.State

import Algebra
import SLanguage
import ProcessTree

--

partial
hd : List a -> a
hd [] = idris_crash "hd"
hd (x :: xs) = x

partial
tl : List a -> List a
tl [] = idris_crash "tl"
tl (x :: xs) = xs

--

Sig : Type
Sig = (Name, List Name)

Sigs : Type
Sigs = SortedMap Nat Sig

isVarTest : Tree -> Node -> Bool
isVarTest tree (b@(MkNode _ _ _ _ (bChId :: _))) =
  isJust $ nodeContr $ getNode tree bChId

isFuncNode : List NodeId -> NodeId -> Bool
isFuncNode fIds nId = nId `elem` fIds

getFGSig : Tree -> String -> NodeId -> Name -> List Name ->
           State (Sigs, List Rule) Sig
getFGSig tree prefix nId name vs =
  do (sigs, rules) <- get
     case the (Maybe Sig) $ lookup nId sigs of
       Nothing =>
         do let name' = prefix ++ strTail name ++
                               (show $ S (length $ toList sigs))
            let sig' = (name', vs)
            let sigs' = insert nId sig' sigs
            put $ (sigs', rules)
            pure sig'
       Just sig' =>
         pure sig'         

putFGRules : List Rule -> State (Sigs, List Rule) ()
putFGRules newRules =
  do (sigs, rules) <- get
     let rules' = rules ++ newRules
     put (sigs, rules')

getChContr : Tree -> List NodeId -> List (Name, List Name)
getChContr tree nIds =
  let children = map (getNode tree) nIds in
  [ (cname, cparams)  |
    MkNode _ _ (Just (MkContraction _ cname cparams)) _ _ <- children]  

mutual

  genResExp : Tree -> List NodeId -> Node -> State (Sigs, List Rule) Exp
  genResExp tree fIds (b@(MkNode _ bE _ _ bChIds)) =
    case findFuncAncestor tree b of
      Nothing =>
        case bE of
          Var _ => pure $ bE
          Call Ctr cname _ =>
            do es <- genResExps tree fIds bChIds
               pure $ Call Ctr cname es
          Call FCall name args =>
            genResCall tree fIds b name args
          Call GCall name args =>
            genResCall tree fIds b name args
          Let _ bs =>
            do let chNode = getNode tree (hd bChIds)
               e' <- genResExp tree fIds chNode
               es' <- genResExps tree fIds (tl bChIds)
               let vnames = map fst bs
               let subst = fromList (vnames `zip` es')
               pure $ applySubst subst e'
      Just (MkNode aId aE aC _ (aChId :: _)) =>
        do (sigs, rules) <- get
           let Just (name, params) = lookup aId sigs
           let args = map Var params
           let Just subst = matchAgainst aE bE
           let aChNode = getNode tree aChId
           case nodeContr aChNode of
             Nothing =>
               pure $ applySubst subst (Call FCall name args)
             Just _ =>
               pure $ applySubst subst (Call GCall name args)

  genResExps : Tree -> List NodeId -> List NodeId ->
                    State (Sigs, List Rule) (List Exp)
  genResExps tree fIds nIds =
    for (map (getNode tree) nIds) (genResExp tree fIds)

  genResCall : Tree -> List NodeId -> Node -> Name -> List Exp ->
                    State (Sigs, List Rule) Exp
  genResCall tree fId (b@(MkNode bId bE _ _ bChIds)) name args =
    let params = vars bE in
    if isVarTest tree b then
      do (sigs, rules) <- get
         (name', _) <- getFGSig tree "g" bId name params
         bodies <- genResExps tree fId bChIds
         let contrs = getChContr tree bChIds
         let grules =
               [GRule name' cname' cparams' (tl params) body' |
                  ((cname', cparams'), body') <- contrs `zip` bodies]
         putFGRules grules                
         pure $ Call GCall name' (map Var params)
    else if isFuncNode fId bId then
      do (sigs, rules) <- get
         (name', params') <- getFGSig tree "f" bId name params
         let bChNode = getNode tree (hd bChIds)
         body' <- genResExp tree fId bChNode
         putFGRules [FRule name' params' body'] 
         pure $ Call FCall name' (map Var params)
    else
      let bChNode = getNode tree (hd bChIds) in
      genResExp tree fId bChNode

genResidualProgram' : Tree -> State (Sigs, List Rule) (Program, Exp)
genResidualProgram' tree =
  do let initNode = getNode tree 0
     let fIds = funcNodeIds tree
     resExp <- genResExp tree fIds initNode
     (_, rules) <- get
     pure (MkProgram rules, resExp)

export
genResidualProgram : Tree -> (Program, Exp)
genResidualProgram tree =
  (evalState $ genResidualProgram' tree) (empty, [])
