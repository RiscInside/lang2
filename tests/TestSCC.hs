module TestSCC (sccTestsList) where

import Data.List.NonEmpty (NonEmpty (..))
import SCC (IVertex (IVertex), computeSCCsTest)
import Test.HUnit (Test (TestCase, TestList), assertEqual)

visitsEverything :: Test
visitsEverything =
  TestCase $
    assertEqual
      "Graph with vertices [1, 2, 3] and edges 2 -> 1 and 1 -> 3 should have 3 separate SCCs"
      [IVertex 3 :| [], IVertex 1 :| [], IVertex 2 :| []]
      (computeSCCsTest [1 .. 3] [(2, 1), (1, 3)])

-- from https://en.wikipedia.org/wiki/Strongly_connected_component#/media/File:Scc-1.svg
graph1 :: [(IVertex, IVertex)]
graph1 = [(1, 2), (2, 3), (3, 1), (1, 4), (4, 5), (5, 4), (6, 5), (6, 7), (7, 6), (7, 8), (8, 7), (3, 8)]

testOnGraph1 :: Test
testOnGraph1 =
  TestCase $
    assertEqual
      "Graph at https://en.wikipedia.org/wiki/Strongly_connected_component#/media/File:Scc-1.svg"
      [5 :| [4], 8 :| [7, 6], 1 :| [2, 3]]
      (computeSCCsTest [1 .. 8] graph1)

testOnGraph1' :: Test
testOnGraph1' =
  TestCase $
    assertEqual
      "Graph at https://en.wikipedia.org/wiki/Strongly_connected_component#/media/File:Scc-1.svg (reversed order)"
      [5 :| [4], 8 :| [7, 6], 3 :| [1, 2]]
      (computeSCCsTest [8, 7, 6, 5, 4, 3, 2, 1] graph1)

sccTestsList :: Test
sccTestsList = TestList [visitsEverything, testOnGraph1, testOnGraph1']
