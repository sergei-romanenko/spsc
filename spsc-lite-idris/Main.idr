module Main

import Data.SortedMap

import SLanguage
import SParsers
--import Algebra
--import HE
--import MSG
--import MSGTests
import ProcessTree
import PTBuilder
import ResProgGen

-- The process tree is returned by the supercompilers
-- just to enable the user to take a look at it.

Supercompiler : Type
Supercompiler = Program -> Exp -> (Tree, Exp, Program)

mkScp : TreeBuilder -> Supercompiler
mkScp builder prog e =
  let tree = basicBuilder prog e
      (resExp, resProg) = genResidualProgram tree
  in (tree, resExp, resProg)

basicScp : Supercompiler
basicScp = mkScp basicBuilder

advancedScp : Supercompiler
advancedScp = mkScp advancedBuilder

mkScpTask : TreeBuilder -> String -> Maybe String
mkScpTask builder task =
  do MkTask e prog <- parseTask task
     let tree = basicBuilder prog e
     let (resExp, resProg) = genResidualProgram tree
     let s = show tree ++ "\n" ++
             show resExp ++ " where " ++
             show resProg
     pure $ s

advancedScpTask : String -> Maybe String
advancedScpTask = mkScpTask advancedBuilder

exampleTask : String
exampleTask = "f(x) where f(x) = A;";

main : IO ()
main = do
  [_, taskName] <- getArgs
    | _ => putStrLn "Usage: spsc-lite-idris taskname"
  let pathTask = taskName ++  ".task"
  let pathOut = taskName ++  ".out"
  Right task <- readFile pathTask
    | Left ferr =>
        putStrLn ("Error reading file " ++ pathTask ++ ": " ++ show ferr)
  putStrLn ("# Task read from " ++ pathTask)
  let Just out = advancedScpTask task
    | Nothing => putStrLn ("Syntax error(s) in " ++ pathTask ++ " !")
  Right _ <- writeFile pathOut out 
    | Left ferr =>
        putStrLn ("Error writing file " ++ pathOut ++ ": " ++ show ferr)
  putStrLn ("# Output written to " ++ pathOut)
