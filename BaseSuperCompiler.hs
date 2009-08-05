module BaseSuperCompiler where

import qualified Data.Map as Map
import Control.Monad.State

import SLanguage
import ShowUtil
import SParsers
import Algebra
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

drivingStep :: Program -> Exp -> State Int [Branch]

drivingStep prog e =
  case e of
    Call Ctr name args ->
      return $ [ (arg, Nothing) | arg <- args  ]
    Call FCall name args ->
      let (params, body) = lookupF prog name
          p2a = Map.fromList(params `zip` args)
          body' = substExp p2a body
      in return $ [(body', Nothing)]
    Call GCall name (Call Ctr cname cargs : args) ->
      let (cparams, params, body) = lookupGC prog name cname
          cp2ca = Map.fromList(cparams `zip` cargs)
          p2a = Map.fromList(params `zip` args)
          body' = substExp (cp2ca `Map.union` p2a) body
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

driveBranch :: Program -> Exp -> Name -> Name -> Params -> Params -> State Int (Exp, Maybe Contraction)
driveBranch prog e vname cname cparams params =
  do cparams' <- freshNameList (length cparams)
     let cargs = map Var cparams'
     let vname2ctr = Map.fromList[(vname, Call Ctr cname cargs)]
     let e' = substExp vname2ctr e
     [(e'', Nothing)] <- drivingStep prog e'
     return $ (e'', Just $ Contraction vname cname cparams')

-- 
--  def buildProcessTree(e: Term): Tree = {
--    var t = new Tree(new Node(e, null, null), Map().withDefaultValue(Nil))
--    while (t.leaves.exists{!_.isProcessed}) {
--      val b = t.leaves.find(!_.isProcessed).get
--      t = b.ancestors.find(a => !trivial(a.expr) && inst(a.expr, b.expr)) match {
--        case Some(a) => t.replace(b, Let(b.expr, findSubst(b.expr, a.expr).toList))
--        case None => t.addChildren(b, driveExp(b.expr)) // drive
--      }
--    }
--    t
--  } 
-- 
--  def freshPat(p: Pat) = Pat(p.name, p.args map freshVar)
