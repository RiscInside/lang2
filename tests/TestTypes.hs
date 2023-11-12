module TestTypes (typesTestsList) where

import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Types (DemoTy (DApp, DVar), unifyTest)

canSolveForVariables :: Test
canSolveForVariables =
  TestCase $
    assertEqual
      "`a ~ Int` should be solved with [a = Int]"
      (unifyTest (DVar "a" 1) (DApp "Int" []))
      (Just (DApp "Int" []), [("a", DApp "Int" [])])

basicOccursCheckFail :: Test
basicOccursCheckFail =
  TestCase $
    assertEqual
      "`Map[a, Int] ~ a` shouldn't be solved since a would be infinite"
      (unifyTest (DApp "Map" [DVar "a" 1, DApp "Int" []]) (DVar "a" 1))
      (Nothing, [])

identityOccursCheckPass :: Test
identityOccursCheckPass =
  TestCase $
    assertEqual
      "`a ~ a` should be solved with identity substitution"
      (unifyTest (DVar "a" 1) (DVar "a" 1))
      (Just (DVar "a" 1), [])

unifyVisitsParameters :: Test
unifyVisitsParameters =
  TestCase $
    assertEqual
      "`Map[Int, a]` ~ `Map[b, String]` should be solved with [a = string, b = int]"
      (unifyTest (DApp "Map" [DApp "Int" [], DVar "a" 1]) (DApp "Map" [DVar "b" 1, DApp "String" []]))
      (Just (DApp "Map" [DApp "Int" [], DApp "String" []]), [("a", DApp "String" []), ("b", DApp "Int" [])])

typesTestsList :: Test
typesTestsList = TestList [canSolveForVariables, basicOccursCheckFail, identityOccursCheckPass, unifyVisitsParameters]
