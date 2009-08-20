'''
Created on Aug 20, 2009

@author: Sergei Romanenko
'''

from sll_language import *
from process_tree import Contraction

class DrivingEngine(object):

    def __init__(self, prog, nameGen):
        "The program is supposed to be correct: no duplicate definitions, etc."
        self.nameGen = nameGen
        self.fRule = dict()
        self.gRules = dict()
        self.gcRule = dict()
        for rule in prog.rules:
            name = rule.name
            if isinstance(rule, FRule):
                self.fRule[name] = rule
            elif isinstance(rule, GRule):
                if name in self.gRules:
                    "Lists are mutable!"
                    self.gRules[name].append(rule)
                else:
                    self.gRules[name] = [rule]
                self.gcRule[(rule.name, rule.cname)] = rule
            else:
                raise ValueError("Invalid rule")

    def drivingStep(self, e):
        if e.isCtr():
            return [(arg, None) for arg in e.args]
        elif e.isFCall():
            rule = self.fRule[e.name]
            p2a = dict(zip(rule.params, e.args))
            body = rule.body.applySubst(p2a)
            return [(body, None)]
        elif e.isGCall() and e.args[0].isCtr():
            cname = e.args[0].name
            cargs = e.args[0].args
            rule = self.gcRule[(e.name, cname)]
            p2a = dict()
            p2a.update(zip(rule.cparams, cargs))
            p2a.update(zip(rule.params, e.args[1:]))
            body = rule.body.applySubst(p2a)
            return [(body, None)]
        elif e.isGCall() and e.args[0].isVar():
            rules = self.gRules[e.name]
            return [ self.driveBranch(e, rule) for rule in rules ]

#    Call GCall name (arg0 : args) ->
#      do branches <- drivingStep prog arg0
#         return $ [ (Call GCall name (e' : args), c) | (e', c) <- branches]
#    Let body bindings ->
#      return $
#        (body, Nothing) : [(e', Nothing) | (_, e') <- bindings]

    def driveBranch(self, e, rule):
        vname = e.args[0].vname
        cname = rule.cname
        cparams = self.nameGen.freshNameList(len(rule.cparams))
        params = rule.params
        cargs = [Var(vn) for vn in cparams]
        vname2ctr = dict([(vname, Ctr(cname, cargs))])
        e1 = e.applySubst(vname2ctr)
        branches = self.drivingStep(e1)
        e2 = branches[0][0]
        return (e2, Contraction(vname, cname, cparams))


#---- The parts common to the basic and advanced supercompilers.
#
#findMoreGeneralAncestors :: Tree -> Node -> [Node]
#
#findMoreGeneralAncestors tree beta@(Node _ eB _ _ _) =
#  [ alpha  | alpha@(Node _ eA _ _ _) <- ancestors tree beta,
#             isFGCall eA, eB `instOf` eA]
#
#unprocessedNodes :: Tree -> [Node]
#
#unprocessedNodes tree =
#  let leaves = map (tree IntMap.!) (treeLeaves tree)
#  in [ leaf | leaf <- leaves, not $ isProcessed tree leaf ]
#
#buildLoop :: (Program -> Tree -> Node -> State Int Tree) ->
#                        Program ->
#                        Tree -> State Int Tree
#
#buildLoop buildStep prog tree =
#  case unprocessedNodes tree of
#    [] -> return tree
#    beta : _ ->
#      do tree' <- buildStep prog tree beta
#         buildLoop buildStep prog tree'
#
#initTree :: Exp -> Tree
#
#initTree e = IntMap.singleton 0 (Node 0 e Nothing Nothing [])
#
#buildProcessTree :: (Program -> Tree -> Node -> State Int Tree) -> 
#                    Program -> Exp -> Tree
#
#buildProcessTree buildStep prog e =
#  (evalState $
#     buildLoop buildStep prog (initTree e))
#    10000 
#
#-- If beta `instOf` alpha, we generalize beta by introducing
#-- a let-expression, in order to make beta the same as alpha
#-- (modulo variable names).
#
#loopBack :: Program -> Tree -> Node -> Node -> State Int Tree
#
#loopBack prog tree beta@(Node bId eB _ _ _) alpha@(Node _ eA _ _ _) =
#  do let Just subst = matchAgainst eA eB
#         bindings = Map.toAscList subst
#         letExp = Let eA bindings
#         tree' = replaceSubtree tree bId letExp
#     return tree'
#
#-- This function applies a driving step to the node's expression,
#-- and, in general, adds children to the node.
#
#expandNode :: Program -> Tree -> Node -> State Int Tree
#
#expandNode prog tree beta@(Node bId eB _ _ _) =
#  do branches <- drivingStep prog eB
#     tree' <- addChildren tree bId branches
#     return tree'
#
#---- Basic supercompiler
#
#basic_buildStep :: Program -> Tree -> Node -> State Int Tree
#
#basic_buildStep prog tree beta =
#  case findMoreGeneralAncestors tree beta of
#    [] -> expandNode prog tree beta
#    alpha : _ -> loopBack prog tree beta alpha
#
#basic_buildProcessTree :: Program -> Exp -> Tree
#
#basic_buildProcessTree prog e =
#  buildProcessTree basic_buildStep prog e
