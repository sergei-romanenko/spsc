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
import PrettyPrinter

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

main : IO ()
main = do
  [_, taskName] <- getArgs
    | _ => putStrLn "Usage: spsc-lite-idris taskname"
  let pathTask = taskName ++ ".task"
  let pathTree = taskName ++ ".tree"
  let pathRes  = taskName ++ ".res"
  Right task <- readFile pathTask
    | Left ferr =>
        putStrLn ("Error reading file " ++ pathTask ++ ": " ++ show ferr)
  putStrLn ("* Task read from " ++ pathTask)
  let Just (MkTask e prog) = parseTask task
    | Nothing => putStrLn ("Syntax error(s) in " ++ pathTask ++ " !")
  let (tree, resExp, resProg) = advancedScp prog e
  Right _ <- writeFile pathTree (ppTree $ tree) 
    | Left ferr =>
        putStrLn ("Error writing file " ++ pathTree ++ ": " ++ show ferr)
  putStrLn ("* Process tree written to " ++ pathTree)
  Right _ <- writeFile pathRes (ppTask $ MkTask resExp resProg)
    | Left ferr =>
        putStrLn ("Error writing file " ++ pathRes ++ ": " ++ show ferr)
  putStrLn ("* Output written to " ++ pathRes)
