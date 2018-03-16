module Main

import Data.SortedMap

import SLanguage
import SParsers
import ProcessTree
import PTBuilder
import ResProgGen
import PrettyPrinter

-- The process tree is returned by the supercompilers
-- just to enable the user to take a look at it.

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
  let tree = advancedBuilder prog e
  Right _ <- writeFile pathTree (ppTree $ tree) 
    | Left ferr =>
        putStrLn ("Error writing file " ++ pathTree ++ ": " ++ show ferr)
  putStrLn ("* Process tree written to " ++ pathTree)
  let (resExp, resProg) = genResidualProgram tree
  Right _ <- writeFile pathRes (ppTask $ MkTask resExp resProg)
    | Left ferr =>
        putStrLn ("Error writing file " ++ pathRes ++ ": " ++ show ferr)
  putStrLn ("* Output written to " ++ pathRes)
