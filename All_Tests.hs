module All_Tests where

import Test.HUnit

import Algebra_Tests
import HE_Tests
import MSG_Tests
import Supercompiler_Tests
import ResidualProgramGenerator_Tests

allTests = TestList [
  algebraTests, heTests, msgTests, drStepTests, scpTests
  ]

runAllTests = runTestTT allTests