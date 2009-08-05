module Main where

import Text.ParserCombinators.Parsec
import Test.HUnit

import SLanguage
import SParsers

import HE_Tests

--main = return()
main = runTestTT heTests

--main =
--  do ast <- parseSLL "f(x)=x;g(X,z)=z;g(C(x,y),z)=D(g(y),E);"
--     print ast
