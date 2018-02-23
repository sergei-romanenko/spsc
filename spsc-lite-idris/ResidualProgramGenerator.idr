module ResidualProgramGenerator

import Data.SortedMap
import Control.Monad.State

import Algebra
import SLanguage
import ProcessTree

--

partial
hd : List a -> a
hd (x :: xs) = x

partial
tl : List a -> List a
tl (x :: xs) = xs

--

Sig : Type
Sig = (Name, List Name)

Sigs : Type
Sigs = SortedMap Nat Sig

isVarTest : Tree -> Node -> Bool
isVarTest tree (b@(MkNode _ _ _ _ (bChId :: _))) =
  maybe False (isJust . nodeContr) (lookup bChId tree)

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
  let children = mapMaybe (`lookup` tree) nIds in
  [ (cname, cparams)  |
    MkNode _ _ (Just (MkContraction _ cname cparams)) _ _ <- children]  

mutual

  genResExp : Tree -> Node -> State (Sigs, List Rule) Exp
  genResExp tree (b@(MkNode _ bE _ _ bChIds)) =
    case funcAncestors tree b of
      [] =>
        case bE of
          Var _ => pure $ bE
          Call Ctr cname _ =>
            do es <- genResExps tree bChIds
               pure $ Call Ctr cname es
          Call FCall name args =>
            genResCall tree b name args
          Call GCall name args =>
            genResCall tree b name args
          Let _ bs =>
            do let Just chId = lookup (hd bChIds) tree
               e' <- genResExp tree chId
               es' <- genResExps tree (tl bChIds)
               let vnames = map fst bs
               let subst = fromList (vnames `zip` es')
               pure $ applySubst subst e'
      (MkNode aId aE aC _ (aChId :: _)) :: _ =>
        do (sigs, rules) <- get
           let Just (name, params) = lookup aId sigs
           let args = map Var params
           let Just subst = matchAgainst aE bE
           let Just aChNode = lookup aChId tree
           case nodeContr aChNode of
             Nothing =>
               pure $ applySubst subst (Call FCall name args)
             Just _ =>
               pure $ applySubst subst (Call GCall name args)

  genResExps : Tree -> List NodeId -> State (Sigs, List Rule) (List Exp)
  genResExps tree nIds =
    for (mapMaybe (`lookup` tree) nIds) (genResExp tree)

  genResCall : Tree -> Node -> Name -> List Exp -> State (Sigs, List Rule) Exp
  genResCall tree (b@(MkNode bId bE _ _ bChIds)) name args =
    let params = vars bE in
    if isVarTest tree b then
      do (sigs, rules) <- get
         (name', _) <- getFGSig tree "g" bId name params
         bodies <- genResExps tree bChIds
         let contrs = getChContr tree bChIds
         let grules =
               [GRule name' cname' cparams' (tl params) body' |
                  ((cname', cparams'), body') <- contrs `zip` bodies]
         putFGRules grules                
         pure $ Call GCall name' (map Var params)
    else if isFuncNode tree bId then
      do (sigs, rules) <- get
         (name', params') <- getFGSig tree "f" bId name params
         let Just bChNode = lookup (hd bChIds) tree
         body' <- genResExp tree bChNode
         putFGRules [FRule name' params' body'] 
         pure $ Call FCall name' (map Var params)
    else
      let Just bChNode = lookup (hd bChIds) tree in
      genResExp tree bChNode

genResidualProgram' : Tree -> State (Sigs, List Rule) (Program, Exp)
genResidualProgram' tree =
  do let Just initNode = lookup 0 tree
     resExp <- genResExp tree initNode
     (_, rules) <- get
     pure (MkProgram rules, resExp)

export
genResidualProgram : Tree -> (Program, Exp)
genResidualProgram tree =
  (evalState $ genResidualProgram' tree) (empty, [])
