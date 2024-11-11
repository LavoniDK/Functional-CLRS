module Main where

import MergeSort (mergeSort, merge)
import Test.HUnit

testMergeSort1 = TestCase (assertEqual "mergeSort test 1" [1, 2, 3, 4, 5, 6] (mergeSort [6, 5, 4, 3, 2, 1] 0 5))
testMergeSort2 = TestCase (assertEqual "mergeSort test 2" [1, 1, 2, 3, 3, 4] (mergeSort [3, 1, 4, 2, 1, 3] 0 5))
testMergeSort3 = TestCase (assertEqual "mergeSort test 3" [1] (mergeSort [1] 0 0))
testMergeSort4 = TestCase (assertEqual "mergeSort test 4" [] (mergeSort [] 0 0))

testMerge1 = TestCase (assertEqual "merge test 1" [1, 2, 3, 4, 5] (merge [1, 3, 5] [2, 4]))
testMerge2 = TestCase (assertEqual "merge test 2" [1, 2, 3] (merge [1] [2, 3]))
testMerge3 = TestCase (assertEqual "merge test 3" [1, 2, 3] (merge [2] [1, 3]))
testMerge4 = TestCase (assertEqual "merge test 4" [1, 2] (merge [] [1, 2]))

tests = TestList [testMergeSort1, testMergeSort2, testMergeSort3, testMergeSort4, 
                 testMerge1, testMerge2, testMerge3, testMerge4]

main :: IO Counts
main = runTestTT tests
