module ResidualProgramGenerator where

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Control.Monad.State

import Algebra
import SLanguage
import ProcessTree

type Sig = (Name, [Name])
type Sigs = IntMap.IntMap Sig

genResidualProgram :: Tree -> (Program, Exp)

genResidualProgram tree =
  (evalState $ genResidualProgram' tree) (IntMap.empty, [])

genResidualProgram' :: Tree -> State (Sigs, [Rule]) (Program, Exp)

genResidualProgram' tree =
  do resExp <- genResPrExp tree (tree IntMap.! 0)
     (_, rules) <- get
     return (Program rules, resExp)

genResPrExp :: Tree -> Node -> State (Sigs, [Rule]) Exp

genResPrExp tree (b@(Node _ bE _ _ bChIds)) =
  case funcAncestors tree b of
    [] ->
      case bE of
        Var _ -> return $ bE
        Call Ctr cname _ ->
          do es <- genResPrExps tree bChIds
             return $ Call Ctr cname es
        Call FCall name args ->
          genResPrCall tree b name args
        Call GCall name args ->
          genResPrCall tree b name args
        Let _ bs ->
          do e' <- genResPrExp tree (tree IntMap.! (head bChIds))
             es' <- genResPrExps tree (tail bChIds)
             let vnames = map fst bs
                 subst = Map.fromList(vnames `zip` es')
             return $ applySubst subst e'
    (Node aId aE aC _ (aChId : _)) : _ ->
      do (sigs, rules) <- get
         let (name, params) = sigs IntMap.! aId
             args = map Var params
             Just subst = matchAgainst aE bE
         case nodeContr (tree IntMap.! aChId) of
           Nothing ->
             return $ applySubst subst (Call FCall name args)
           Just _ ->
             return $ applySubst subst (Call GCall name args)

genResPrExps :: Tree -> [NodeId] -> State (Sigs, [Rule]) [Exp]

genResPrExps tree nIds =
  mapM (genResPrExp tree) (map (tree IntMap.!) nIds)       

isVarTest :: Tree -> Node -> Bool

isVarTest tree (b@(Node _ _ _ _ (bChId : _))) =
  case nodeContr(tree IntMap.! bChId) of
    Nothing -> False
    Just _ -> True

getFGSig :: Tree -> String -> NodeId -> Name -> [Name] ->
              State (Sigs, [Rule]) Sig

getFGSig tree prefix nId name vs =
  do (sigs, rules) <- get
     case IntMap.lookup nId sigs of
       Nothing ->
         do let name' = prefix ++ tail name ++ show (IntMap.size sigs + 1)
                sig' = (name', vs)
                sigs' = IntMap.insert nId sig' sigs
            put $ (sigs', rules)
            return sig'
       Just sig' ->
         return sig'         

putFGRules :: [Rule] -> State (Sigs, [Rule]) ()

putFGRules newRules =
  do (sigs, rules) <-get
     let rules' = rules ++ newRules
     put (sigs, rules')

getChContr :: Tree -> [NodeId] -> [(Name, [Name])]

getChContr tree nIds =
  let children = map (tree IntMap.!) nIds
  in [ (cname, cparams)  |
         Node _ _ (Just (Contraction _ cname cparams)) _ _ <- children]  

genResPrCall :: Tree -> Node -> Name -> [Exp] ->
                  State (Sigs, [Rule]) Exp

genResPrCall tree (b@(Node bId bE _ _ bChIds)) name args =
  let params = vars bE
  in if isVarTest tree b then
       do (sigs, rules) <- get
          (name', _) <- getFGSig tree "g" bId name params
          bodies <- genResPrExps tree bChIds
          let contrs = getChContr tree bChIds
          let grules =
                [GRule name' cname' cparams' (tail params) body' |
                   ((cname', cparams'), body') <- contrs `zip` bodies]
          putFGRules grules                
          return $ Call GCall name' (map Var params)
     else if isFuncNode tree bId then
       do (sigs, rules) <- get
          (name', params') <- getFGSig tree "f" bId name params
          body' <- genResPrExp tree (tree IntMap.! (head bChIds))
          putFGRules [FRule name' params' body'] 
          return $ Call GCall name' (map Var params)
     else
       genResPrExp tree (tree IntMap.! head bChIds)
