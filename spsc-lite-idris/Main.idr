module Main

import Data.SortedMap

import SLanguage
--import SLanguageTests
import SParsers
--import Algebra
--import AlgebraTests
--import HE
--import HETests
--import MSG
--import MSGTests
import ProcessTree
import PTBuilder
--import PTBuilderTests
import ResProgGen
--import ResProgGenTests

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
  putStrLn "Hello! I'm SPSC Lite in Idirs!"
  let Just res = advancedScpTask exampleTask {- | Nothing => "" -}
  putStrLn res
