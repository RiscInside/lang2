module Main where

import System.Exit
import Test.HUnit
import TestSCC (sccTestsList)
import TestTypes (typesTestsList)

tests :: Test
tests = TestList [TestLabel "unification tests" typesTestsList, TestLabel "SCC tests" sccTestsList]

main :: IO ()
main = runTestTTAndExit tests
